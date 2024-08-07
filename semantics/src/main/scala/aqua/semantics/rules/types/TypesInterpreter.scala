/*
 * Copyright (C) 2024  Fluence DAO
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package aqua.semantics.rules.types

import aqua.errors.Errors.internalError
import aqua.helpers.data.{PName, SName}
import aqua.parser.lexer.*
import aqua.raw.ops.Call
import aqua.raw.value.*
import aqua.semantics.Levenshtein
import aqua.semantics.rules.StackInterpreter
import aqua.semantics.rules.locations.{DefinitionInfo, LocationsAlgebra}
import aqua.semantics.rules.report.ReportAlgebra
import aqua.semantics.rules.types.TypeResolution.TypeResolutionError
import aqua.types.*
import aqua.types.Type.*

import cats.Applicative
import cats.data.*
import cats.data.Validated.{Invalid, Valid}
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import monocle.Lens
import monocle.macros.GenLens
import scala.annotation.tailrec
import scala.collection.immutable.SortedMap
import scala.reflect.TypeTest

class TypesInterpreter[S[_], X](using
  lens: Lens[X, TypesState[S]],
  report: ReportAlgebra[S, State[X, *]],
  locations: LocationsAlgebra[S, State[X, *]]
) extends TypesAlgebra[S, State[X, *]] {

  val stack = new StackInterpreter[S, X, TypesState[S], TypesState.Frame[S]](
    GenLens[TypesState[S]](_.stack)
  )

  import stack.*

  type ST[A] = State[X, A]

  override def resolveType(
    token: TypeToken[S],
    mustBeDefined: Boolean = true
  ): State[X, Option[Type]] =
    getState.map(TypeResolution.resolveTypeToken(token)).flatMap {
      case Valid(TypeResolution(typ, tokens)) =>
        val tokensLocs = tokens.map { case (t, n) => n -> t }
        locations.pointLocations(tokensLocs).as(typ.some)
      case Invalid(errors) if mustBeDefined =>
        errors.traverse_ { case TypeResolutionError(token, hint) =>
          report.error(token, hint)
        }.as(none)
      case _ => none.pure
    }

  override def resolveStreamType(token: TypeToken[S]): State[X, Option[MutableStreamType]] =
    OptionT(resolveType(token)).flatMapF {
      case st: MutableStreamType => st.some.pure[ST]
      case t => report.error(token, s"Expected stream or stream map type, got $t").as(none)
    }.value

  def resolveNamedType(token: TypeToken[S]): State[X, Option[AbilityType | StructType]] =
    resolveType(token).flatMap(_.flatTraverse {
      case t: (AbilityType | StructType) => Option(t).pure
      case _ => report.error(token, "Type must be an ability or a data").as(None)
    })

  override def resolveArrowDef(arrowDef: ArrowTypeToken[S]): State[X, Option[ArrowType]] =
    getState.map(TypeResolution.resolveArrowDef(arrowDef)).flatMap {
      case Valid(TypeResolution(tt, tokens)) =>
        val tokensLocs = tokens.map { case (t, n) => n -> t }
        locations.pointLocations(tokensLocs).as(tt.some)
      case Invalid(errs) =>
        errs.traverse_ { case TypeResolutionError(token, hint) =>
          report.error(token, hint)
        }.as(none)
    }

  override def resolveServiceType(
    name: NamedTypeToken[S],
    mustBeDefined: Boolean = true
  ): State[X, Option[ServiceType]] =
    resolveType(name, mustBeDefined).flatMap {
      case Some(serviceType: ServiceType) =>
        serviceType.some.pure
      case Some(t) if mustBeDefined =>
        report.error(name, s"Type `$t` is not a service").as(none)
      case None if mustBeDefined =>
        report.error(name, s"Type `${name.value}` is not defined").as(none)
      case _ => none.pure
    }

  override def defineAbilityType(
    name: NamedTypeToken[S],
    fields: Map[String, (Name[S], Type)]
  ): State[X, Option[AbilityType]] =
    ensureNameNotDefined(name.value, name, ifDefined = none) {
      val types = fields.view.mapValues { case (_, t) => t }.toMap
      NonEmptyMap
        .fromMap(SortedMap.from(types))
        .fold(report.error(name, s"Ability `${name.value}` has no fields").as(none))(
          nonEmptyFields =>
            val `type` = AbilityType(name.value, nonEmptyFields)

            locateNamedType(name, `type`, fields) >> modify(_.defineType(name, `type`))
              .as(`type`.some)
        )
    }

  override def defineServiceType(
    name: NamedTypeToken[S],
    fields: Map[String, (Name[S], Type)]
  ): State[X, Option[ServiceType]] =
    ensureNameNotDefined(name.value, name, ifDefined = none)(
      fields.toList.traverse {
        case (field, (fieldName, t: ArrowType)) =>
          OptionT
            .when(t.codomain.length <= 1)(field -> t)
            .flatTapNone(
              report.error(fieldName, "Service functions cannot have multiple results")
            )
        case (field, (fieldName, t)) =>
          OptionT(
            report
              .error(
                fieldName,
                s"Field '$field' has unacceptable for service field type '$t'"
              )
              .as(none)
          )
      }.flatMapF(arrows =>
        NonEmptyMap
          .fromMap(SortedMap.from(arrows))
          .fold(
            report.error(name, s"Service `${name.value}` has no fields").as(none)
          )(_.some.pure)
      ).semiflatMap(nonEmptyArrows =>
        val `type` = ServiceType(name.value, nonEmptyArrows)

        locateNamedType(name, `type`, fields) >> modify(_.defineType(name, `type`)).as(`type`)
      ).value
    )

  private def locateNamedType(
    name: NamedTypeToken[S],
    t: NamedType,
    fields: Map[String, (Name[S], Type)]
  ) = locations.addDefinitionWithFields(
    DefinitionInfo[S](name.pathName, name, t),
    fields.map { case (n, (t, ty)) => DefinitionInfo[S](PName.simpleUnsafe(n), t, ty) }.toList
  )

  override def defineStructType(
    name: NamedTypeToken[S],
    fields: Map[String, (Name[S], Type)]
  ): State[X, Option[StructType]] =
    ensureNameNotDefined(name.value, name, ifDefined = none)(
      fields.toList.traverse {
        case (field, (_, t: DataType)) =>
          (field -> t).some.pure[ST]
        case (field, (fieldName, t)) =>
          report
            .error(
              fieldName,
              s"Field '$field' has unacceptable for struct field type '$t'"
            )
            .as(none)
      }.map(_.sequence.map(_.toMap))
        .flatMap(
          _.map(SortedMap.from)
            .flatMap(NonEmptyMap.fromMap)
            .fold(
              report.error(name, s"Struct `${name.value}` has no fields").as(none)
            )(nonEmptyFields =>
              val `type` = StructType(name.value, nonEmptyFields)

              locateNamedType(name, `type`, fields) >> modify(_.defineType(name, `type`))
                .as(`type`.some)
            )
        )
    )

  override def defineAlias(name: NamedTypeToken[S], target: Type): State[X, Boolean] =
    getState.map(_.getType(name.value)).flatMap {
      case Some(_) => report.error(name, s"Type `${name.value}` was already defined").as(false)
      case None =>
        modify(_.defineType(name, target))
          .productL(locations.addDefinition(DefinitionInfo(name.pathName, name, target)))
          .as(true)
    }

  override def resolveIntoField(
    op: IntoField[S],
    rootT: Type
  ): State[X, Option[IntoFieldRes]] = {
    rootT match {
      case nt: NamedType =>
        nt.fields(op.value) match {
          case Some(t) =>
            locations
              .pointFieldLocation(
                // TODO: Refactor so that `nt.name` is PName
                PName.stringUnsafe(nt.name),
                op.simpleName,
                op
              )
              .as(Some(IntoFieldRes.Field(t)))
          case None =>
            val fields = nt.fields.keys.map(k => s"`$k`").toList.mkString(", ")
            report
              .error(
                op,
                s"Field `${op.value}` not found in type `${nt.name}`, available: $fields"
              )
              .as(None)
        }
      case t =>
        t.properties
          .get(op.value) match {
          case Some(t) =>
            State.pure(Some(IntoFieldRes.Property(t)))
          case None =>
            report
              .error(
                op,
                s"Property `${op.value}` not found in type `$t`"
              )
              .as(None)
        }
    }
  }

  private def checkArrowType(
    op: IntoArrow[S],
    tOp: Option[Type],
    rootName: String,
    availableStr: String,
    types: List[Type]
  ): State[X, Option[ArrowType]] = {
    val opName = op.name.value
    tOp match {
      case Some(at: ArrowType) =>
        val reportNotEnoughArguments =
          /* Report at position of arrow application */
          report
            .error(
              op,
              s"Not enough arguments for arrow `$opName` in `$rootName`, " +
                s"expected: ${at.domain.length}, given: ${op.arguments.length}"
            )
            .whenA(op.arguments.length < at.domain.length)
        val reportTooManyArguments =
          /* Report once at position of the first extra argument */
          op.arguments.drop(at.domain.length).headOption.traverse_ { arg =>
            report
              .error(
                arg,
                s"Too many arguments for arrow `$opName` in `$rootName`, " +
                  s"expected: ${at.domain.length}, given: ${op.arguments.length}"
              )
          }
        val checkArgumentTypes =
          op.arguments
            .zip(types)
            .zip(at.domain.toList)
            .forallM { case ((arg, argType), expectedType) =>
              ensureTypeMatches(arg, expectedType, argType)
            }

        reportNotEnoughArguments *>
          reportTooManyArguments *>
          checkArgumentTypes.map(typesMatch =>
            Option.when(
              typesMatch && at.domain.length == op.arguments.length
            )(at)
          )

      case Some(t) =>
        report
          .error(op, s"Field `$opName` has non arrow type `$t` in `$rootName`")
          .as(None)
      case None =>
        report
          .error(op, s"Arrow `$opName` not found in `$rootName`, available: $availableStr")
          .as(None)
    }
  }

  override def resolveIntoArrow(
    op: IntoArrow[S],
    rootT: Type,
    types: List[Type]
  ): State[X, Option[ArrowType]] = {
    /* Safeguard to check condition on arguments */
    if (op.arguments.length != types.length)
      internalError(s"Invalid arguments, lists do not match: ${op.arguments} and $types")

    val opName = op.name.value

    rootT match {
      case ab: GeneralAbilityType =>
        val abName = ab.fullName
        val avStr = ab.arrowFields.keys.map(k => s"`$k`").mkString(", ")
        locations.pointFieldLocation(PName.stringUnsafe(ab.name), op.simpleName, op) *>
          checkArrowType(op, ab.fields.lookup(opName), abName, avStr, types)
      case st: StreamMapType =>
        val avStr = StreamMapType.allFuncs.map(k => s"`${k.name}`").mkString(", ")
        checkArrowType(op, st.funcByString(opName), st.toString, avStr, types)
      case t =>
        /* NOTE: Arrows are only supported on services and abilities,
           (`.copy(...)` for structs is resolved by separate method) */
        report
          .error(op, s"Arrow `$opName` not found in `$t`")
          .as(None)
    }
  }

  override def resolveIntoCopy(
    op: IntoCopy[S],
    rootT: Type,
    types: NonEmptyList[Type]
  ): State[X, Option[StructType]] = {
    if (op.args.length != types.length)
      internalError(s"Invalid arguments, lists do not match: ${op.args} and $types")

    rootT match {
      case st: StructType =>
        op.args
          .zip(types)
          .forallM { case (arg, argType) =>
            val fieldName = arg.argName.value
            st.fields.lookup(fieldName) match {
              case Some(fieldType) =>
                ensureTypeMatches(arg.argValue, fieldType, argType)
              case None =>
                report
                  .error(
                    arg.argName,
                    s"No field with name '$fieldName' in `$st`"
                  )
                  .as(false)
            }
          }
          .map(Option.when(_)(st))
      case t =>
        report
          .error(
            op,
            s"Non data type `$t` does not support `.copy`"
          )
          .as(None)
    }
  }

  override def resolveIntoIndex(
    op: IntoIndex[S],
    rootT: Type,
    idxType: Type
  ): State[X, Option[DataType]] =
    ensureTypeOneOf(
      op.idx.getOrElse(op),
      ScalarType.integer,
      idxType
    ) *> (rootT match {
      case ot: OptionType =>
        op.idx.fold(State.pure(Some(ot.element)))(v =>
          // TODO: Is this a right place to report this error?
          // It is not a type error, but rather a syntax error
          report.error(v, s"Options might have only one element, use ! to get it").as(None)
        )
      case rt: CollectionType =>
        State.pure(Some(rt.element))
      case t =>
        report
          .error(
            op,
            s"Non collection type `$t` does not support indexing"
          )
          .as(None)
    })

  override def ensureValuesComparable(
    token: Token[S],
    left: Type,
    right: Type
  ): State[X, Boolean] = {
    // TODO: This needs more comprehensive logic
    @tailrec
    def isComparable(lt: Type, rt: Type): Boolean =
      (lt, rt) match {
        // All numbers are comparable
        case (lst: ScalarType, rst: ScalarType)
            if ScalarType.number(lst) && ScalarType.number(rst) =>
          true
        // Hack: u64 `U` LiteralType.signed = TopType,
        // but they should be comparable
        case (lst: ScalarType, LiteralType.signed) if ScalarType.number(lst) =>
          true
        case (LiteralType.signed, rst: ScalarType) if ScalarType.number(rst) =>
          true
        case (lbt: CollectionType, rbt: CollectionType) =>
          isComparable(lbt.element, rbt.element)
        // Prohibit comparing abilities
        case (_: AbilityType, _: AbilityType) =>
          false
        // Prohibit comparing arrows
        case (_: ArrowType, _: ArrowType) =>
          false
        case (LiteralType(xs, _), LiteralType(ys, _)) =>
          xs.intersect(ys).nonEmpty
        case _ =>
          lt `∪` rt != TopType
      }

    if (isComparable(left, right)) State.pure(true)
    else report.error(token, s"Cannot compare '$left' with '$right''").as(false)
  }

  override def ensureTypeMatches(
    token: Token[S],
    expected: Type,
    givenType: Type
  ): State[X, Boolean] =
    if (expected.acceptsValueOf(givenType)) State.pure(true)
    else {
      (expected, givenType) match {
        case (valueNamedType: NamedType, typeNamedType: NamedType) =>
          val valueFields = valueNamedType.fields
          val typeFields = typeNamedType.fields
          // value can have more fields
          if (valueFields.length < typeFields.length) {
            report
              .error(
                token,
                "Number of fields doesn't match, " +
                  s"expected: $expected, given: $givenType"
              )
              .as(false)
          } else {
            valueFields.toSortedMap.toList.forallM { (name, `type`) =>
              typeFields.lookup(name) match {
                case Some(t) =>
                  val nextToken = token match {
                    case NamedValueToken(_, fields) =>
                      fields.find(_.argName.value == name).getOrElse(token)
                    // TODO: Is it needed?
                    case PropertyToken(_, properties) =>
                      properties.last
                    case _ => token
                  }
                  ensureTypeMatches(nextToken, `type`, t)
                case None =>
                  report
                    .error(
                      token,
                      s"Wrong value type, expected: $expected, given: $givenType"
                    )
                    .as(false)
              }
            }
          }
        case _ =>
          val notes = (expected, givenType) match {
            case (_, dt: DataType) if expected.acceptsValueOf(OptionType(dt)) =>
              "note: Try converting value to optional" :: Nil
            case (dt: DataType, _) if givenType.acceptsValueOf(OptionType(dt)) =>
              "note: You're providing an optional value where normal value is expected." ::
                "You can extract value with `!`, but be aware it may trigger join behaviour." ::
                Nil
            case _ => Nil
          }

          report
            .error(
              token,
              "Types mismatch." +:
                s"expected:   $expected" +:
                s"given:      $givenType" +:
                notes
            )
            .as(false)
      }
    }

  override def ensureTypeConstructibleFrom(
    token: Token[S],
    expected: AbilityType | StructType,
    arguments: NonEmptyMap[String, (NamedArg[S], Type)]
  ): State[X, Boolean] = for {
    /* Check that required fields are present
       among arguments and have correct types */
    enough <- expected.fields.toNel.forallM { case (name, typ) =>
      arguments.lookup(name) match {
        case Some(arg -> givenType) =>
          ensureTypeMatches(arg.argValue, typ, givenType)
        case None =>
          report
            .error(
              token,
              s"Missing argument '$name' of type '$typ'"
            )
            .as(false)
      }
    }
    expectedKeys = expected.fields.keys.toNonEmptyList
    /* Report unexpected arguments */
    _ <- arguments.toNel.traverse_ { case (name, arg -> _) =>
      expected.fields.lookup(name) match {
        case Some(_) => State.pure(())
        case None =>
          lazy val similar = Levenshtein
            .mostSimilar(name, expectedKeys, 3)
            .map(s => s"'$s'")
            .mkString(", ")
          val message =
            if (enough)
              s"Unexpected argument '$name'"
            else
              s"Unexpected argument '$name', did you mean $similar?"

          report.warning(arg.argName, message)
      }
    }
  } yield enough

  private def typeTo[T <: Type](
    token: Token[S],
    givenType: Type,
    error: String
  )(using tt: TypeTest[Type, T]): OptionT[State[X, *], T] =
    givenType match {
      case t: T => OptionT.pure(t)
      case _ =>
        OptionT.liftF(
          report.error(token, error)
        ) *> OptionT.none
    }

  override def typeToCollectible(
    token: Token[S],
    givenType: Type
  ): OptionT[State[X, *], CollectibleType] =
    typeTo[CollectibleType](
      token,
      givenType,
      s"Value of type '$givenType' could not be put into a collection"
    )

  override def typeToStream(
    token: Token[S],
    givenType: Type
  ): OptionT[State[X, *], StreamType] =
    typeTo[StreamType](
      token,
      givenType,
      s"Expected stream value (*), got value of type '$givenType'"
    )

  override def typeToStreamMap(
    token: Token[S],
    givenType: Type
  ): OptionT[State[X, *], StreamMapType] =
    typeTo[StreamMapType](
      token,
      givenType,
      s"Expected stream map value (%), got value of type '$givenType'"
    )

  override def typeToIterable(
    token: Token[S],
    givenType: Type
  ): OptionT[State[X, *], CollectionType] =
    typeTo[CollectionType](
      token,
      givenType,
      s"Value of type '$givenType' could not be iterated over"
    )

  override def ensureTypeOneOf[T <: Type](
    token: Token[S],
    expected: Set[T],
    givenType: Type
  ): State[X, Option[Type]] = expected
    .find(_ acceptsValueOf givenType)
    .fold(
      report
        .error(
          token,
          "Types mismatch." ::
            s"expected one of:   ${expected.mkString(", ")}" ::
            s"given:             $givenType" :: Nil
        )
        .as(none)
    )(_.some.pure)

  override def checkArrowCallResults(
    token: Token[S],
    arrowType: ArrowType,
    results: List[Name[S]],
    exports: List[Call.Export]
  ): State[X, Unit] = for {
    _ <- results
      .drop(arrowType.codomain.length)
      .traverse_(result =>
        report
          .error(
            result,
            "Types mismatch. Cannot assign to a variable " +
              "the result of a call that returns nothing"
          )
      )
    _ <- report
      .warning(
        token,
        s"Arrow returns ${arrowType.codomain.length match {
          case 0 => "no values"
          case 1 => "a value"
          case i => s"$i values"
        }}, but ${results.length match {
          case 0 => "none are"
          case 1 => "only one is"
          case i => s"only $i are"
        }} used"
      )
      .whenA(checkNumberOfResults(arrowType, results, exports))
  } yield ()

  private def checkNumberOfResults(
    at: ArrowType,
    results: List[Name[S]],
    exports: List[Call.Export]
  ): Boolean = {
    val checkLength = at.codomain.length > results.length
    val isOneStreamMapInExport =
      exports.headOption.exists(e => isStreamMapType(e.`type`)) && exports.length == 1
    val twoResultsToStreamMap = isOneStreamMapInExport && at.codomain.length == 2

    !twoResultsToStreamMap && checkLength
  }

  override def checkArgumentsNumber(
    token: Token[S],
    expected: Int,
    givenNum: Int
  ): State[X, Boolean] =
    if (expected == givenNum) State.pure(true)
    else
      report
        .error(
          token,
          s"Number of arguments doesn't match the function type, expected: $expected, given: $givenNum"
        )
        .as(false)

  override def beginArrowScope(token: ArrowTypeToken[S]): State[X, ArrowType] =
    Applicative[ST]
      .product(
        // Collect argument types
        token.absWithArgs
          .foldLeft(Chain.empty[(String, Type)].pure[ST]) {
            case (f, (Some(argName), argType)) =>
              f.flatMap(acc =>
                // Resolve arg type, remember it
                resolveType(argType).map {
                  case Some(t) => acc.append(argName.value -> t)
                  case None => acc
                }
              )
            // Unnamed argument
            case (f, _) => f
          }
          .map(_.toList)
          .map(ProductType.labelled),
        // Resolve return type
        token.res
          .foldLeft[ST[List[Type]]](Nil.pure[ST])((f, t) =>
            f.flatMap(ts => resolveType(t).map(ts.prependedAll))
          )
          .map(_.reverse)
          .map(ProductType(_))
      )
      .map(argsAndRes => ArrowType(argsAndRes._1, argsAndRes._2))
      .flatMap(at => stack.beginScope(TypesState.Frame(token, at, None)).as(at))

  override def checkArrowReturn(
    values: NonEmptyList[(ValueToken[S], ValueRaw)]
  ): State[X, Boolean] =
    mapStackHeadM[Boolean](
      report
        .error(values.head._1, "Fatal: checkArrowReturn has no matching beginArrowScope")
        .as(false)
    )(frame =>
      if (frame.retVals.nonEmpty)
        report
          .error(
            values.head._1,
            "Return expression was already used in scope; " +
              "you can use only one Return in an arrow declaration, " +
              "use conditional return pattern if you need to return based on condition"
          )
          .as(frame -> false)
      else if (frame.token.res.isEmpty)
        report
          .error(
            values.head._1,
            "No return type declared for this arrow, " +
              "please remove `<- ...` expression " +
              "or add `-> ...` return type(s) declaration to the arrow"
          )
          .as(frame -> false)
      else if (frame.token.res.length > values.length)
        report
          .error(
            values.last._1,
            s"Expected ${frame.token.res.length - values.length} more " +
              s"values to be returned, see return type declaration"
          )
          .as(frame -> false)
      else if (frame.token.res.length < values.length)
        report
          .error(
            values.toList.drop(frame.token.res.length).headOption.getOrElse(values.last)._1,
            s"Too many values are returned from this arrow, " +
              s"this is unexpected. Defined return type:  ${frame.arrowType.codomain}"
          )
          .as(frame -> false)
      else
        frame.arrowType.codomain.toList
          .zip(values.toList)
          .traverse { case (returnType, (token, returnValue)) =>
            if (!returnType.acceptsValueOf(returnValue.`type`))
              report
                .error(
                  token,
                  s"Wrong value type, expected: $returnType, given: ${returnValue.`type`}"
                )
                .as(none)
            else returnValue.some.pure[SX]
          }
          .map(_.sequence)
          .map(res => frame.copy(retVals = res) -> res.isDefined)
    )

  override def endArrowScope(token: Token[S]): State[X, List[ValueRaw]] =
    mapStackHeadM(
      report.error(token, "Fatal: endArrowScope has no matching beginArrowScope").as(Nil)
    )(frame =>
      if (frame.token.res.isEmpty) (frame -> Nil).pure
      else if (frame.retVals.isEmpty)
        report
          .error(
            frame.token.res.headOption.getOrElse(frame.token),
            "Return type is defined for the arrow, but nothing returned. Use `<- value, ...` as the last expression inside function body."
          )
          .as(frame -> Nil)
      else (frame -> frame.retVals.getOrElse(Nil)).pure
    ) <* stack.endScope

  private def ensureNameNotDefined[A](
    name: String,
    token: Token[S],
    ifDefined: => A
  )(
    ifNotDefined: => State[X, A]
  ): State[X, A] = getState
    .map(_.getType(name))
    .flatMap {
      case Some(_) =>
        // TODO: Point to both locations here
        report
          .error(
            token,
            s"Name `$name` was already defined here"
          )
          .as(ifDefined)
      case None => ifNotDefined
    }
}
