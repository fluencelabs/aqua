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
import aqua.semantics.rules.errors.ReportErrors
import aqua.types.{
  ArrayType,
  ArrowType,
  BoxType,
  LiteralType,
  NamedType,
  OptionType,
  ProductType,
  ScalarType,
  ScopeType,
  StreamType,
  StructType,
  Type
}
import cats.data.Validated.{Invalid, Valid}
import cats.data.{Chain, NonEmptyList, NonEmptyMap, State}
import cats.instances.list.*
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import cats.{~>, Applicative}
import monocle.Lens
import monocle.macros.GenLens

import scala.collection.immutable.SortedMap

class TypesInterpreter[S[_], X](implicit
  lens: Lens[X, TypesState[S]],
  error: ReportErrors[S, X],
  locations: LocationsAlgebra[S, State[X, *]]
) extends TypesAlgebra[S, State[X, *]] {

  val stack = new StackInterpreter[S, X, TypesState[S], TypesState.Frame[S]](
    GenLens[TypesState[S]](_.stack)
  )

  import stack.*

  type ST[A] = State[X, A]

  val resolver: (TypesState[S], NamedTypeToken[S]) => Option[
    (Type, List[(Token[S], NamedTypeToken[S])])
  ] = { (state, ctt) =>
    state.strict.get(ctt.value).map(t => (t, state.definitions.get(ctt.value).toList.map(ctt -> _)))
  }

  override def resolveType(token: TypeToken[S]): State[X, Option[Type]] =
    getState.map(st => TypesStateHelper.resolveTypeToken(token, st, resolver)).flatMap {
      case Some(t) =>
        val (tt, tokens) = t
        val tokensLocs = tokens.map { case (t, n) =>
          n.value -> t
        }
        locations.pointLocations(tokensLocs).map(_ => Some(tt))
      case None => report(token, s"Unresolved type").as(None)
    }

  override def resolveArrowDef(arrowDef: ArrowTypeToken[S]): State[X, Option[ArrowType]] =
    getState.map(st => TypesStateHelper.resolveArrowDef(arrowDef, st, resolver)).flatMap {
      case Valid(t) =>
        val (tt, tokens) = t
        val tokensLocs = tokens.map { case (t, n) =>
          n.value -> t
        }
        locations.pointLocations(tokensLocs).map(_ => Some(tt))
      case Invalid(errs) =>
        errs
          .foldLeft[ST[Option[ArrowType]]](State.pure(None)) { case (n, (tkn, hint)) =>
            report(tkn, hint) >> n
          }
    }

  override def defineNamedType(
    name: NamedTypeToken[S],
    `type`: Type
  ): State[X, Boolean] =
    getState.map(_.definitions.get(name.value)).flatMap {
      case Some(n) if n == name => State.pure(true)
      case Some(_) =>
        report(name, s"Type `${name.value}` was already defined").as(false)
      case None =>
        modify { st =>
          st.copy(
            strict = st.strict.updated(name.value, `type`),
            definitions = st.definitions.updated(name.value, name)
          )
        }.as(true)
    }

  override def defineAlias(name: NamedTypeToken[S], target: Type): State[X, Boolean] =
    getState.map(_.definitions.get(name.value)).flatMap {
      case Some(n) if n == name => State.pure(false)
      case Some(_) => report(name, s"Type `${name.value}` was already defined").as(false)
      case None =>
        modify(st =>
          st.copy(
            strict = st.strict.updated(name.value, target),
            definitions = st.definitions.updated(name.value, name)
          )
        ).flatMap(_ => locations.addToken(name.value, name)).as(true)
    }

  override def resolveField(rootT: Type, op: IntoField[S]): State[X, Option[PropertyRaw]] = {
    rootT match {
      case nt: NamedType =>
        nt.fields(op.value)
          .fold(
            report(
              op,
              s"Field `${op.value}` not found in type `${nt.name}`, available: ${nt.fields.toNel.toList.map(_._1).mkString(", ")}"
            ).as(None)
          ) { t =>
            println(s"type for ${op.value} is $t")
            locations.pointFieldLocation(nt.name, op.value, op).as(Some(IntoFieldRaw(op.value, t)))
          }
      case t =>
        t.properties
          .get(op.value)
          .fold(
            report(
              op,
              s"Expected data type to resolve a field '${op.value}' or a type with this property. Got: $rootT"
            ).as(None)
          )(t => State.pure(Some(FunctorRaw(op.value, t))))

    }
  }

  override def resolveArrow(
    rootT: Type,
    op: IntoArrow[S],
    arguments: List[ValueRaw]
  ): State[X, Option[PropertyRaw]] = {
    rootT match {
      case ScopeType(name, fieldsAndArrows) =>
        fieldsAndArrows(op.name.value).fold(
          report(
            op,
            s"Arrow `${op.name.value}` not found in type `$name`, available: ${fieldsAndArrows.toNel.toList.map(_._1).mkString(", ")}"
          ).as(None)
        ) { t =>
          val resolvedType = t match {
            // TODO: is it a correct way to resolve `IntoArrow` type?
            case ArrowType(_, codomain) => codomain.uncons.map(_._1).getOrElse(t)
            case _ => t
          }
          println(s"resolve arrow for ${op.name.value} with type: ${resolvedType}")
          locations
            .pointFieldLocation(name, op.name.value, op)
            .as(Some(IntoArrowRaw(op.name.value, resolvedType, arguments)))
        }
      case t =>
        t.properties
          .get(op.name.value)
          .fold(
            report(
              op,
              s"Expected scope type to resolve an arrow '${op.name.value}' or a type with this property. Got: $rootT"
            ).as(None)
          )(t => State.pure(Some(FunctorRaw(op.name.value, t))))

    }
  }

  // TODO actually it's stateless, exists there just for reporting needs
  override def resolveCopy(
    rootT: Type,
    op: IntoCopy[S],
    fields: NonEmptyMap[String, ValueRaw]
  ): State[X, Option[PropertyRaw]] =
    rootT match {
      case st: StructType =>
        fields.toSortedMap.toList.traverse { case (fieldName, value) =>
          st.fields.lookup(fieldName) match {
            case Some(t) =>
              ensureTypeMatches(op.fields.lookup(fieldName).getOrElse(op), t, value.`type`)
            case None => report(op, s"No field with name '$fieldName' in $rootT").as(false)
          }
        }.map(res => if (res.forall(identity)) Some(IntoCopyRaw(st, fields)) else None)

      case _ =>
        report(op, s"Expected $rootT to be a data type").as(None)
    }

  // TODO actually it's stateless, exists there just for reporting needs
  override def resolveIndex(
    rootT: Type,
    op: IntoIndex[S],
    idx: ValueRaw
  ): State[X, Option[PropertyRaw]] =
    if (!ScalarType.i64.acceptsValueOf(idx.`type`))
      report(op, s"Expected numeric index, got $idx").as(None)
    else
      rootT match {
        case ot: OptionType =>
          op.idx.fold(
            State.pure(Some(IntoIndexRaw(idx, ot.element)))
          )(v => report(v, s"Options might have only one element, use ! to get it").as(None))
        case rt: BoxType =>
          State.pure(Some(IntoIndexRaw(idx, rt.element)))
        case _ =>
          report(op, s"Expected $rootT to be a collection type").as(None)
      }

  override def ensureValuesComparable(
    token: Token[S],
    left: Type,
    right: Type
  ): State[X, Boolean] = {
    val isComparable = (left, right) match {
      case (LiteralType(xs, _), LiteralType(ys, _)) =>
        xs.intersect(ys).nonEmpty
      case _ =>
        left.acceptsValueOf(right)
    }

    if (isComparable) State.pure(true)
    else
      report(token, s"Cannot compare '$left' with '$right''")
        .as(false)
  }

  private def extractToken(token: Token[S]) =
    token match {
      case VarToken(n, properties) => properties.lastOption.getOrElse(n)
      case t => t
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
            report(
              token,
              s"Number of fields doesn't match the data type, expected: $expected, given: $givenType"
            ).as(false)
          } else {
            valueFields.toSortedMap.toList.traverse { (name, `type`) =>
              typeFields.lookup(name) match {
                case Some(t) =>
                  val nextToken = extractToken(token match {
                    case NamedValueToken(_, fields) =>
                      fields.lookup(name).getOrElse(token)
                    case t => t
                  })
                  ensureTypeMatches(nextToken, `type`, t)
                case None =>
                  report(
                    token,
                    s"Wrong value type, expected: $expected, given: $givenType"
                  ).as(false)
              }
            }.map(_.forall(identity))
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
          reportError(
            token,
            "Types mismatch." :: s"expected:   $expected" :: s"given:      $givenType" :: Nil ++ notes
          )
            .as(false)
      }
    }

  override def expectNoExport(token: Token[S]): State[X, Unit] =
    report(
      token,
      "Types mismatch. Cannot assign to a variable the result of a call that returns nothing"
    ).as(())

  override def checkArgumentsNumber(
    token: Token[S],
    expected: Int,
    givenNum: Int
  ): State[X, Boolean] =
    if (expected == givenNum) State.pure(true)
    else
      report(
        token,
        s"Number of arguments doesn't match the function type, expected: ${expected}, given: $givenNum"
      ).as(false)

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
    mapStackHeadE[Boolean](
      report(values.head._1, "Fatal: checkArrowReturn has no matching beginArrowScope").as(false)
    )((frame: TypesState.Frame[S]) =>
      if (frame.retVals.nonEmpty)
        Left(
          (
            values.head._1,
            "Return expression was already used in scope; you can use only one Return in an arrow declaration, use conditional return pattern if you need to return based on condition",
            false
          )
        )
      else if (frame.token.res.isEmpty)
        Left(
          (
            values.head._1,
            "No return type declared for this arrow, please remove `<- ...` expression or add `-> ...` return type(s) declaration to the arrow",
            false
          )
        )
      else if (frame.token.res.length > values.length)
        Left(
          (
            values.last._1,
            s"Expected ${frame.token.res.length - values.length} more values to be returned, see return type declaration",
            false
          )
        )
      else if (frame.token.res.length < values.length)
        Left(
          (
            values.toList.drop(frame.token.res.length).headOption.getOrElse(values.last)._1,
            s"Too many values are returned from this arrow, this one is unexpected. Defined return type:  ${frame.arrowType.codomain}",
            false
          )
        )
      else {
        frame.arrowType.codomain.toList
          .lazyZip(values.toList)
          .foldLeft[Either[(Token[S], String, Boolean), List[ValueRaw]]](Right(Nil)) {
            case (acc, (returnType, (_, returnValue))) =>
              acc.flatMap { a =>
                if (!returnType.acceptsValueOf(returnValue.`type`))
                  Left(
                    (
                      values.toList
                        .drop(frame.token.res.length)
                        .headOption
                        .getOrElse(values.last)
                        ._1,
                      s"Wrong value type, expected: $returnType, given: ${returnValue.`type`}",
                      false
                    )
                  )
                else Right(a :+ returnValue)
              }
          }
          .map(res => frame.copy(retVals = Some(res)) -> true)
      }
    )

  override def endArrowScope(token: Token[S]): State[X, List[ValueRaw]] =
    mapStackHeadE[List[ValueRaw]](
      report(token, "Fatal: endArrowScope has no matching beginArrowScope").as(Nil)
    )(frame =>
      if (frame.token.res.isEmpty) {
        Right(frame -> Nil)
      } else if (frame.retVals.isEmpty) {
        Left(
          (
            frame.token.res.headOption.getOrElse(frame.token),
            "Return type is defined for the arrow, but nothing returned. Use `<- value, ...` as the last expression inside function body.",
            Nil
          )
        )
      } else Right(frame -> frame.retVals.getOrElse(Nil))
    ) <* stack.endScope
}
