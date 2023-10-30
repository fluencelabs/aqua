package aqua.semantics.rules.types

import aqua.parser.lexer.*
import aqua.raw.value.{
  FunctorRaw,
  IntoArrowRaw,
  IntoCopyRaw,
  IntoFieldRaw,
  IntoIndexRaw,
  PropertyRaw,
  ValueRaw
}
import aqua.semantics.rules.locations.LocationsAlgebra
import aqua.semantics.rules.StackInterpreter
import aqua.semantics.rules.report.ReportAlgebra
import aqua.semantics.rules.types.TypesStateHelper.{TypeResolution, TypeResolutionError}
import aqua.types.*

import cats.data.Validated.{Invalid, Valid}
import cats.data.{Chain, NonEmptyList, NonEmptyMap, OptionT, State}
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import cats.syntax.foldable.*
import cats.{~>, Applicative}
import cats.syntax.option.*
import monocle.Lens
import monocle.macros.GenLens

import scala.collection.immutable.SortedMap

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

  override def getType(name: String): State[X, Option[Type]] =
    getState.map(st => st.strict.get(name))

  override def resolveType(token: TypeToken[S]): State[X, Option[Type]] =
    getState.map(TypesStateHelper.resolveTypeToken(token)).flatMap {
      case Some(TypeResolution(typ, tokens)) =>
        val tokensLocs = tokens.map { case (t, n) => n.value -> t }
        locations.pointLocations(tokensLocs).as(typ.some)
      case None =>
        // TODO: Give more specific error message
        report.error(token, s"Unresolved type").as(None)
    }

  def resolveNamedType(token: TypeToken[S]): State[X, Option[AbilityType | StructType]] =
    resolveType(token).flatMap(_.flatTraverse {
      case t: (AbilityType | StructType) => Option(t).pure
      case _ => report.error(token, "Type must be an ability or a data").as(None)
    })

  override def resolveArrowDef(arrowDef: ArrowTypeToken[S]): State[X, Option[ArrowType]] =
    getState.map(TypesStateHelper.resolveArrowDef(arrowDef)).flatMap {
      case Valid(TypeResolution(tt, tokens)) =>
        val tokensLocs = tokens.map { case (t, n) => n.value -> t }
        locations.pointLocations(tokensLocs).as(tt.some)
      case Invalid(errs) =>
        errs.traverse_ { case TypeResolutionError(token, hint) =>
          report.error(token, hint)
        }.as(none)
    }

  override def resolveServiceType(name: NamedTypeToken[S]): State[X, Option[ServiceType]] =
    resolveType(name).flatMap {
      case Some(serviceType: ServiceType) =>
        serviceType.some.pure
      case Some(t) =>
        report.error(name, s"Type `$t` is not a service").as(none)
      case None =>
        report.error(name, s"Type `${name.value}` is not defined").as(none)
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

            modify(_.defineType(name, `type`)).as(`type`.some)
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

        modify(_.defineType(name, `type`)).as(`type`)
      ).value
    )

  override def defineStructType(
    name: NamedTypeToken[S],
    fields: Map[String, (Name[S], Type)]
  ): State[X, Option[StructType]] =
    ensureNameNotDefined(name.value, name, ifDefined = none)(
      fields.toList.traverse {
        case (field, (fieldName, t: DataType)) =>
          t match {
            case _: StreamType =>
              report.error(fieldName, s"Field '$field' has stream type").as(none)
            case _ => (field -> t).some.pure[ST]
          }
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

              modify(_.defineType(name, `type`)).as(`type`.some)
            )
        )
    )

  override def defineAlias(name: NamedTypeToken[S], target: Type): State[X, Boolean] =
    getState.map(_.definitions.get(name.value)).flatMap {
      case Some(n) if n == name => State.pure(false)
      case Some(_) => report.error(name, s"Type `${name.value}` was already defined").as(false)
      case None =>
        modify(_.defineType(name, target))
          .productL(locations.addToken(name.value, name))
          .as(true)
    }

  override def resolveField(rootT: Type, op: IntoField[S]): State[X, Option[PropertyRaw]] = {
    rootT match {
      case nt: NamedType =>
        nt.fields(op.value)
          .fold(
            report
              .error(
                op,
                s"Field `${op.value}` not found in type `${nt.name}`, available: ${nt.fields.toNel.toList.map(_._1).mkString(", ")}"
              )
              .as(None)
          ) { t =>
            locations.pointFieldLocation(nt.name, op.value, op).as(Some(IntoFieldRaw(op.value, t)))
          }
      case t =>
        t.properties
          .get(op.value)
          .fold(
            report
              .error(
                op,
                s"Expected data type to resolve a field '${op.value}' or a type with this property. Got: $rootT"
              )
              .as(None)
          )(t => State.pure(Some(FunctorRaw(op.value, t))))

    }
  }

  override def resolveArrow(
    rootT: Type,
    op: IntoArrow[S],
    arguments: List[ValueRaw]
  ): State[X, Option[PropertyRaw]] = {
    rootT match {
      case AbilityType(name, fieldsAndArrows) =>
        fieldsAndArrows(op.name.value).fold(
          report
            .error(
              op,
              s"Arrow `${op.name.value}` not found in type `$name`, available: ${fieldsAndArrows.toNel.toList.map(_._1).mkString(", ")}"
            )
            .as(None)
        ) {
          case at @ ArrowType(_, _) =>
            locations
              .pointFieldLocation(name, op.name.value, op)
              .as(Some(IntoArrowRaw(op.name.value, at, arguments)))
          case _ =>
            report
              .error(
                op,
                s"Unexpected. `${op.name.value}` must be an arrow."
              )
              .as(None)
        }
      case t =>
        t.properties
          .get(op.name.value)
          .fold(
            report
              .error(
                op,
                s"Expected scope type to resolve an arrow '${op.name.value}' or a type with this property. Got: $rootT"
              )
              .as(None)
          )(t => State.pure(Some(FunctorRaw(op.name.value, t))))

    }
  }

  // TODO actually it's stateless, exists there just for reporting needs
  override def resolveCopy(
    token: IntoCopy[S],
    rootT: Type,
    args: NonEmptyList[(NamedArg[S], ValueRaw)]
  ): State[X, Option[PropertyRaw]] =
    rootT match {
      case st: StructType =>
        args.forallM { case (arg, value) =>
          val fieldName = arg.argName.value
          st.fields.lookup(fieldName) match {
            case Some(t) =>
              ensureTypeMatches(arg.argValue, t, value.`type`)
            case None =>
              report.error(arg.argName, s"No field with name '$fieldName' in $rootT").as(false)
          }
        }.map(
          Option.when(_)(
            IntoCopyRaw(
              st,
              args.map { case (arg, value) =>
                arg.argName.value -> value
              }.toNem
            )
          )
        )

      case _ =>
        report.error(token, s"Expected $rootT to be a data type").as(None)
    }

  // TODO actually it's stateless, exists there just for reporting needs
  override def resolveIndex(
    rootT: Type,
    op: IntoIndex[S],
    idx: ValueRaw
  ): State[X, Option[PropertyRaw]] =
    if (!ScalarType.i64.acceptsValueOf(idx.`type`))
      report.error(op, s"Expected numeric index, got $idx").as(None)
    else
      rootT match {
        case ot: OptionType =>
          op.idx.fold(
            State.pure(Some(IntoIndexRaw(idx, ot.element)))
          )(v => report.error(v, s"Options might have only one element, use ! to get it").as(None))
        case rt: BoxType =>
          State.pure(Some(IntoIndexRaw(idx, rt.element)))
        case _ =>
          report.error(op, s"Expected $rootT to be a collection type").as(None)
      }

  override def ensureValuesComparable(
    token: Token[S],
    left: Type,
    right: Type
  ): State[X, Boolean] = {
    // TODO: This needs more comprehensive logic
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
        case (lbt: BoxType, rbt: BoxType) =>
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
          lt.uniteTop(rt) != TopType
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
                s"Number of fields doesn't match the data type, expected: $expected, given: $givenType"
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
          val notes =
            if (expected.acceptsValueOf(OptionType(givenType)))
              "note: Try converting value to optional" :: Nil
            else if (givenType.acceptsValueOf(OptionType(expected)))
              "note: You're providing an optional value where normal value is expected." ::
                "You can extract value with `!`, but be aware it may trigger join behaviour." ::
                Nil
            else Nil
          report
            .error(
              token,
              "Types mismatch." :: s"expected:   $expected" :: s"given:      $givenType" :: Nil ++ notes
            )
            .as(false)
      }
    }

  override def ensureTypeIsCollectible(token: Token[S], givenType: Type): State[X, Boolean] =
    givenType match {
      case _: DataType => true.pure
      case _ =>
        report
          .error(
            token,
            s"Value of type '$givenType' could not be put into a collection"
          )
          .as(false)
    }

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
    results: List[Name[S]]
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
      .whenA(arrowType.codomain.length > results.length)
  } yield ()

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
          s"Number of arguments doesn't match the function type, expected: ${expected}, given: $givenNum"
        )
        .as(false)

  override def beginArrowScope(token: ArrowTypeToken[S]): State[X, ArrowType] =
    Applicative[ST]
      .product(
        // Collect argument types
        token.args
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
            "Return expression was already used in scope; you can use only one Return in an arrow declaration, use conditional return pattern if you need to return based on condition"
          )
          .as(frame -> false)
      else if (frame.token.res.isEmpty)
        report
          .error(
            values.head._1,
            "No return type declared for this arrow, please remove `<- ...` expression or add `-> ...` return type(s) declaration to the arrow"
          )
          .as(frame -> false)
      else if (frame.token.res.length > values.length)
        report
          .error(
            values.last._1,
            s"Expected ${frame.token.res.length - values.length} more values to be returned, see return type declaration"
          )
          .as(frame -> false)
      else if (frame.token.res.length < values.length)
        report
          .error(
            values.toList.drop(frame.token.res.length).headOption.getOrElse(values.last)._1,
            s"Too many values are returned from this arrow, this one is unexpected. Defined return type:  ${frame.arrowType.codomain}"
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
    .map(_.definitions.get(name))
    .flatMap {
      case Some(_) =>
        // TODO: Point to both locations here
        report
          .error(
            token,
            s"Name `${name}` was already defined here"
          )
          .as(ifDefined)
      case None => ifNotDefined
    }
}
