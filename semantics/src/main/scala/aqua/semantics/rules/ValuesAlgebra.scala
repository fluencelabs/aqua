package aqua.semantics.rules

import aqua.parser.lexer.*
import aqua.parser.lexer.InfixToken.{BoolOp, CmpOp, MathOp, Op}
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
import scribe.Logging

import scala.collection.immutable.SortedMap

class ValuesAlgebra[S[_], Alg[_]: Monad](implicit
  N: NamesAlgebra[S, Alg],
  T: TypesAlgebra[S, Alg],
  A: AbilitiesAlgebra[S, Alg]
) extends Logging {

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

      case pr @ PrefixToken(o, _) => ???

      case it @ InfixToken(l, r, _) =>
        (valueToRaw(l), valueToRaw(r)).flatMapN {
          case (Some(leftRaw), Some(rightRaw)) =>
            val lType = leftRaw.`type`
            val rType = rightRaw.`type`
            lazy val uType = lType `∪` rType

            it.op match {
              case Op.Bool(bop) =>
                for {
                  leftChecked <- T.ensureTypeMatches(l, ScalarType.bool, lType)
                  rightChecked <- T.ensureTypeMatches(r, ScalarType.bool, rType)
                } yield Option.when(
                  leftChecked && rightChecked
                )(
                  ApplyBinaryOpRaw(
                    op = bop match {
                      case BoolOp.And => ApplyBinaryOpRaw.Op.And
                      case BoolOp.Or => ApplyBinaryOpRaw.Op.Or
                    },
                    left = leftRaw,
                    right = rightRaw
                  )
                )
              case op @ (Op.Math(_) | Op.Cmp(_)) =>
                // Some type acrobatics to make
                // compiler check exhaustive pattern matching
                val iop = op match {
                  case Op.Math(op) => op
                  case Op.Cmp(op) => op
                }

                val hasFloat = List(lType, rType).exists(
                  _ acceptsValueOf LiteralType.float
                )

                // See https://github.com/fluencelabs/aqua-lib/blob/main/math.aqua
                val (id, fn) = iop match {
                  case MathOp.Add => ("math", "add")
                  case MathOp.Sub => ("math", "sub")
                  case MathOp.Mul if hasFloat => ("math", "fmul")
                  case MathOp.Mul => ("math", "mul")
                  case MathOp.Div => ("math", "div")
                  case MathOp.Rem => ("math", "rem")
                  case MathOp.Pow => ("math", "pow")
                  case CmpOp.Gt => ("cmp", "gt")
                  case CmpOp.Gte => ("cmp", "gte")
                  case CmpOp.Lt => ("cmp", "lt")
                  case CmpOp.Lte => ("cmp", "lte")
                }

                /*
                 * If `uType == TopType`, it means that we don't
                 * have type big enough to hold the result of operation.
                 * e.g. We will use `i64` for result of `i32 * u64`
                 * TODO: Handle this more gracefully
                 *       (use warning system when it is implemented)
                 */
                def uTypeBounded = if (uType == TopType) {
                  val bounded = ScalarType.i64
                  logger.warn(
                    s"Result type of ($lType ${it.op} $rType) is $TopType, " +
                      s"using $bounded instead"
                  )
                  bounded
                } else uType

                // Expected type sets of left and right operands, result type
                val (leftExp, rightExp, resType) = iop match {
                  case MathOp.Add | MathOp.Sub | MathOp.Div | MathOp.Rem =>
                    (ScalarType.integer, ScalarType.integer, uTypeBounded)
                  case MathOp.Pow =>
                    (ScalarType.integer, ScalarType.unsigned, uTypeBounded)
                  case MathOp.Mul if hasFloat =>
                    (ScalarType.float, ScalarType.float, ScalarType.i64)
                  case MathOp.Mul =>
                    (ScalarType.integer, ScalarType.integer, uTypeBounded)
                  case CmpOp.Gt | CmpOp.Lt | CmpOp.Gte | CmpOp.Lte =>
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

            }
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
