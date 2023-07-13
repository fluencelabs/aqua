package aqua.semantics.rules

import aqua.parser.lexer.*
import aqua.parser.lexer.InfixToken.Op
import aqua.raw.value.*
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.types.*
import cats.Monad
import cats.data.Chain
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import cats.syntax.option.*
import cats.instances.list.*
import cats.data.{NonEmptyList, NonEmptyMap}

import scala.collection.immutable.SortedMap

class ValuesAlgebra[S[_], Alg[_]: Monad](implicit
  N: NamesAlgebra[S, Alg],
  T: TypesAlgebra[S, Alg],
  A: AbilitiesAlgebra[S, Alg]
) {

  def ensureIsString(v: ValueToken[S]): Alg[Boolean] =
    ensureTypeMatches(v, LiteralType.string)

  def ensureTypeMatches(v: ValueToken[S], expected: Type): Alg[Boolean] =
    resolveType(v).flatMap {
      case Some(vt) =>
        T.ensureTypeMatches(
          v,
          expected,
          vt
        )
      case None => false.pure[Alg]
    }

  def resolveType(v: ValueToken[S]): Alg[Option[Type]] =
    valueToRaw(v).map(_.map(_.`type`))

  private def resolveSingleProperty(rootType: Type, op: PropertyOp[S]): Alg[Option[PropertyRaw]] =
    op match {
      case op: IntoField[S] =>
        T.resolveField(rootType, op)
      case op: IntoArrow[S] =>
        op.arguments
          .map(valueToRaw)
          .sequence
          .map(_.sequence)
          .flatMap {
            case None => None.pure[Alg]
            case Some(arguments) => T.resolveArrow(rootType, op, arguments)
          }
      case op: IntoCopy[S] =>
        op.fields
          .map(valueToRaw)
          .sequence
          .map(_.sequence)
          .flatMap {
            case None => None.pure[Alg]
            case Some(values) => T.resolveCopy(rootType, op, values)
          }
      case op: IntoIndex[S] =>
        op.idx
          .fold[Alg[Option[ValueRaw]]](Option(LiteralRaw.Zero).pure[Alg])(
            valueToRaw
          )
          .flatMap {
            case None => None.pure[Alg]
            case Some(values) => T.resolveIndex(rootType, op, values)
          }
    }

  def valueToRaw(v: ValueToken[S]): Alg[Option[ValueRaw]] =
    v match {
      case l: LiteralToken[S] => Some(LiteralRaw(l.value, l.ts)).pure[Alg]
      case VarToken(name, ops) =>
        N.read(name).flatMap {
          case Some(t) =>
            // Prepare property expression: take the last known type and the next op, add next op to accumulator
            ops
              .foldLeft[Alg[(Option[Type], Chain[PropertyRaw])]](
                (Some(t) -> Chain.empty).pure[Alg]
              ) { case (acc, op) =>
                acc.flatMap {
                  // Some(rootType) means that the previous property op was resolved successfully
                  case (Some(rootType), prop) =>
                    // Resolve a single property
                    resolveSingleProperty(rootType, op).map {
                      // Property op resolved, add it to accumulator and update the last known type
                      case Some(p) => (Some(p.`type`), prop :+ p)
                      // Property op is not resolved, it's an error, stop iterations
                      case None => (None, Chain.empty)
                    }

                  // We have already errored, do nothing
                  case _ => (None, Chain.empty).pure[Alg]
                }

              }
              .map {
                // Some(_) means no errors occured
                case (Some(_), property) if property.length == ops.length =>
                  Some(property.foldLeft[ValueRaw](VarRaw(name.value, t)) { case (v, p) =>
                    ApplyPropertyRaw(v, p)
                  })

                case _ => None
              }

          case None =>
            None.pure[Alg]
        }

      case dvt @ NamedValueToken(typeName, fields) =>
        T.resolveType(typeName).flatMap {
          case Some(resolvedType) =>
            for {
              fieldsRawOp: NonEmptyMap[String, Option[ValueRaw]] <- fields.traverse(valueToRaw)
              fieldsRaw: List[(String, ValueRaw)] = fieldsRawOp.toSortedMap.toList.collect {
                case (n, Some(vr)) => n -> vr
              }
              rawFields = NonEmptyMap.fromMap(SortedMap.from(fieldsRaw))
              typeFromFieldsWithData = rawFields
                .map(rf =>
                  resolvedType match {
                    case struct @ StructType(_, _) =>
                      (
                        StructType(typeName.value, rf.map(_.`type`)),
                        Some(MakeStructRaw(rf, struct))
                      )
                    case scope @ AbilityType(_, _) =>
                      (
                        AbilityType(typeName.value, rf.map(_.`type`)),
                        Some(AbilityRaw(rf, scope))
                      )
                  }
                )
                .getOrElse(BottomType -> None)
              (typeFromFields, data) = typeFromFieldsWithData
              isTypesCompatible <- T.ensureTypeMatches(dvt, resolvedType, typeFromFields)
            } yield data.filter(_ => isTypesCompatible)
          case _ => None.pure[Alg]
        }

      case ct @ CollectionToken(_, values) =>
        values.traverse(valueToRaw).map(_.flatten).map(NonEmptyList.fromList).map {
          case Some(raws) if raws.size == values.size =>
            val element = raws.map(_.`type`).reduceLeft(_ `∩` _)
            // In case we mix values of uncomparable types, intersection returns bottom, meaning "uninhabited type".
            // But we want to get to TopType instead: this would mean that intersection is empty, and you cannot
            // make any decision about the structure of type, but can push anything inside
            val elementNotBottom = if (element == BottomType) TopType else element
            Some(
              CollectionRaw(
                raws,
                ct.mode match {
                  case CollectionToken.Mode.StreamMode => StreamType(elementNotBottom)
                  case CollectionToken.Mode.ArrayMode => ArrayType(elementNotBottom)
                  case CollectionToken.Mode.OptionMode => OptionType(elementNotBottom)
                }
              )
            )
          case _ if values.isEmpty => Some(ValueRaw.Nil)
          case _ => None
        }

      case ca: CallArrowToken[S] =>
        callArrowToRaw(ca).map(_.widen[ValueRaw])

      case it @ InfixToken(l, r, _) =>
        (valueToRaw(l), valueToRaw(r)).flatMapN {
          case (Some(leftRaw), Some(rightRaw)) =>
            val lType = leftRaw.`type`
            val rType = rightRaw.`type`
            lazy val uType = lType `∪` rType
            val hasFloat = List(lType, rType).exists(
              _ acceptsValueOf LiteralType.float
            )

            // See https://github.com/fluencelabs/aqua-lib/blob/main/math.aqua
            val (id, fn) = it.op match {
              case Op.Add => ("math", "add")
              case Op.Sub => ("math", "sub")
              case Op.Mul if hasFloat => ("math", "fmul")
              case Op.Mul => ("math", "mul")
              case Op.Div => ("math", "div")
              case Op.Rem => ("math", "rem")
              case Op.Pow => ("math", "pow")
              case Op.Gt => ("cmp", "gt")
              case Op.Gte => ("cmp", "gte")
              case Op.Lt => ("cmp", "lt")
              case Op.Lte => ("cmp", "lte")
            }

            // Expected type sets of left and right operands, result type
            val (leftExp, rightExp, resType) = it.op match {
              case Op.Add | Op.Sub | Op.Div | Op.Rem =>
                (ScalarType.integer, ScalarType.integer, uType)
              case Op.Pow =>
                (ScalarType.integer, ScalarType.unsigned, uType)
              case Op.Mul if hasFloat =>
                (ScalarType.float, ScalarType.float, ScalarType.i64)
              case Op.Mul =>
                (ScalarType.integer, ScalarType.integer, uType)
              case Op.Gt | Op.Lt | Op.Gte | Op.Lte =>
                (ScalarType.integer, ScalarType.integer, ScalarType.bool)
            }

            for {
              leftChecked <- T.ensureTypeOneOf(l, leftExp, lType)
              rightChecked <- T.ensureTypeOneOf(r, rightExp, rType)
            } yield Option.when(
              leftChecked.isDefined && rightChecked.isDefined
            )(
              CallArrowRaw(
                ability = Some(id),
                name = fn,
                arguments = leftRaw :: rightRaw :: Nil,
                baseType = ArrowType(
                  ProductType(lType :: rType :: Nil),
                  ProductType(resType :: Nil)
                ),
                serviceId = Some(LiteralRaw.quote(id))
              )
            )

          case _ => None.pure[Alg]
        }

    }

  // Generate CallArrowRaw for arrow in ability
  def callAbType(
    ab: String,
    abType: AbilityType,
    ca: CallArrowToken[S]
  ): Alg[Option[CallArrowRaw]] =
    abType.arrows.get(ca.funcName.value) match {
      case Some(arrowType) =>
        Option(CallArrowRaw(None, s"$ab.${ca.funcName.value}", Nil, arrowType, None)).pure[Alg]
      case None => None.pure[Alg]
    }

  def callArrowToRaw(ca: CallArrowToken[S]): Alg[Option[CallArrowRaw]] = {
    for {
      raw <- ca.ability
        .fold(
          N.readArrow(ca.funcName)
            .map(
              _.map(bt =>
                CallArrowRaw(
                  ability = None,
                  name = ca.funcName.value,
                  arguments = Nil,
                  baseType = bt,
                  serviceId = None
                )
              )
            )
        )(ab =>
          // TODO: Hack. Check that we have registered ability type.
          // If it exists - this is ability type in file, if not - imported ability
          T.getType(ab.value).flatMap {
            case Some(abType: AbilityType) =>
              callAbType(ab.value, abType, ca)
            case _ =>
              (A.getArrow(ab, ca.funcName), A.getServiceId(ab)).mapN {
                case (Some(at), Right(sid)) =>
                  // Service call, actually
                  CallArrowRaw(
                    ability = Some(ab.value),
                    name = ca.funcName.value,
                    arguments = Nil,
                    baseType = at,
                    serviceId = Some(sid)
                  ).some
                case (Some(at), Left(true)) =>
                  // Ability function call, actually
                  CallArrowRaw(
                    ability = Some(ab.value),
                    name = ca.funcName.value,
                    arguments = Nil,
                    baseType = at,
                    serviceId = None
                  ).some
                case _ => none
              }
          }
        )
      result <- raw.flatTraverse(r =>
        val arr = r.baseType
        for {
          argsCheck <- T.checkArgumentsNumber(ca.funcName, arr.domain.length, ca.args.length)
          args <- Option
            .when(argsCheck)(ca.args zip arr.domain.toList)
            .traverse(
              _.flatTraverse { case (tkn, tp) =>
                for {
                  maybeValueRaw <- valueToRaw(tkn)
                  checked <- maybeValueRaw.flatTraverse(v =>
                    T.ensureTypeMatches(tkn, tp, v.`type`)
                      .map(Option.when(_)(v))
                  )
                } yield checked.toList
              }
            )
          result = args
            .filter(_.length == arr.domain.length)
            .map(args => r.copy(arguments = args))
        } yield result
      )
    } yield result
  }

}

object ValuesAlgebra {

  implicit def deriveValuesAlgebra[S[_], Alg[_]: Monad](implicit
    N: NamesAlgebra[S, Alg],
    T: TypesAlgebra[S, Alg],
    A: AbilitiesAlgebra[S, Alg]
  ): ValuesAlgebra[S, Alg] =
    new ValuesAlgebra[S, Alg]
}
