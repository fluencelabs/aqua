package aqua.semantics.rules

import aqua.parser.lexer.*
import aqua.parser.lexer.InfixToken.{BoolOp, CmpOp, EqOp, MathOp, Op as InfOp}
import aqua.parser.lexer.PrefixToken.Op as PrefOp
import aqua.raw.value.*
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.semantics.rules.errors.ErrorsAlgebra
import aqua.types.*

import cats.Monad
import cats.data.Chain
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import cats.syntax.foldable.*
import cats.syntax.option.*
import cats.instances.list.*
import cats.data.{NonEmptyList, NonEmptyMap}
import cats.data.OptionT
import scribe.Logging

import scala.collection.immutable.SortedMap

class ValuesAlgebra[S[_], Alg[_]: Monad](using
  N: NamesAlgebra[S, Alg],
  T: TypesAlgebra[S, Alg],
  E: ErrorsAlgebra[S, Alg],
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
        for {
          maybeArgs <- op.arguments.traverse(valueToRaw)
          arrowProp <- maybeArgs.sequence.flatTraverse(
            T.resolveArrow(rootType, op, _)
          )
        } yield arrowProp
      case op: IntoCopy[S] =>
        for {
          maybeFields <- op.fields.traverse(valueToRaw)
          copyProp <- maybeFields.sequence.flatTraverse(
            T.resolveCopy(rootType, op, _)
          )
        } yield copyProp
      case op: IntoIndex[S] =>
        for {
          maybeIdx <- op.idx.fold(LiteralRaw.Zero.some.pure)(valueToRaw)
          idxProp <- maybeIdx.flatTraverse(
            T.resolveIndex(rootType, op, _)
          )
        } yield idxProp
    }

  def valueToRaw(v: ValueToken[S]): Alg[Option[ValueRaw]] =
    v match {
      case l @ LiteralToken(value, t) =>
        LiteralRaw(l.value, t).some.pure[Alg]

      case VarToken(name) =>
        N.read(name).flatMap {
          case Some(t) =>
            VarRaw(name.value, t).some.pure[Alg]
          case None =>
            None.pure[Alg]
        }

      case prop @ PropertyToken(value, properties) =>
        prop.adjust.fold(
          for {
            valueRaw <- valueToRaw(value)
            result <- valueRaw.flatTraverse(raw =>
              properties
                .foldLeftM(raw) { case (prev, op) =>
                  OptionT(
                    resolveSingleProperty(prev.`type`, op)
                  ).map(prop => ApplyPropertyRaw(prev, prop))
                }
                .value
            )
          } yield result
        )(valueToRaw)

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
        for {
          maybeValuesRaw <- values.traverse(valueToRaw).map(_.sequence)
          valuesRawChecked <- maybeValuesRaw.flatTraverse(raws =>
            raws
              .zip(values)
              .traverse { case (raw, token) =>
                T.ensureTypeIsCollectible(token, raw.`type`)
                  .map(Option.when(_)(raw))
              }
              .map(_.sequence)
          )
          raw = valuesRawChecked.map(raws =>
            NonEmptyList
              .fromList(raws)
              .fold(ValueRaw.Nil) { nonEmpty =>
                val element = raws.map(_.`type`).reduceLeft(_ `∩` _)
                // In case we mix values of uncomparable types, intersection returns bottom, meaning "uninhabited type".
                // But we want to get to TopType instead: this would mean that intersection is empty, and you cannot
                // make any decision about the structure of type, but can push anything inside
                val elementNotBottom = if (element == BottomType) TopType else element
                CollectionRaw(
                  nonEmpty,
                  ct.mode match {
                    case CollectionToken.Mode.StreamMode => StreamType(elementNotBottom)
                    case CollectionToken.Mode.ArrayMode => ArrayType(elementNotBottom)
                    case CollectionToken.Mode.OptionMode => OptionType(elementNotBottom)
                  }
                )
              }
          )
        } yield raw

      case ca: CallArrowToken[S] =>
        callArrowToRaw(ca).map(_.widen[ValueRaw])

      case pr @ PrefixToken(operand, _) =>
        (for {
          raw <- OptionT(
            valueToRaw(operand)
          )
          typeCheck <- OptionT.liftF(
            T.ensureTypeMatches(operand, ScalarType.bool, raw.`type`)
          )
          result <- OptionT.when(typeCheck)(
            ApplyUnaryOpRaw(
              op = pr.op match {
                case PrefOp.Not => ApplyUnaryOpRaw.Op.Not
              },
              value = raw
            )
          )
        } yield result).value

      case it @ InfixToken(l, r, _) =>
        (valueToRaw(l), valueToRaw(r)).flatMapN {
          case (Some(leftRaw), Some(rightRaw)) =>
            val lType = leftRaw.`type`
            val rType = rightRaw.`type`
            lazy val uType = lType `∪` rType

            it.op match {
              case InfOp.Bool(bop) =>
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
              case InfOp.Eq(eop) =>
                T.ensureValuesComparable(
                  token = it,
                  left = lType,
                  right = rType
                ).map(
                  Option.when(_)(
                    ApplyBinaryOpRaw(
                      op = eop match {
                        case EqOp.Eq => ApplyBinaryOpRaw.Op.Eq
                        case EqOp.Neq => ApplyBinaryOpRaw.Op.Neq
                      },
                      left = leftRaw,
                      right = rightRaw
                    )
                  )
                )
              case op @ (InfOp.Math(_) | InfOp.Cmp(_)) =>
                // Some type acrobatics to make
                // compiler check exhaustive pattern matching
                val iop = op match {
                  case InfOp.Math(op) => op
                  case InfOp.Cmp(op) => op
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

  def valueToCallArrowRaw(v: ValueToken[S]): Alg[Option[CallArrowRaw]] =
    valueToRaw(v).flatMap(
      _.flatTraverse {
        case ca: CallArrowRaw => ca.some.pure[Alg]
        // TODO: better error message (`raw` formatting)
        case raw => E.report(v, s"Expected arrow call, got $raw").as(none)
      }
    )

  private def callArrowFromAbility(
    ab: Name[S],
    at: AbilityType,
    funcName: Name[S]
  ): Option[CallArrowRaw] = at.arrows
    .get(funcName.value)
    .map(arrowType =>
      CallArrowRaw.ability(
        ab.value,
        funcName.value,
        arrowType
      )
    )

  private def callArrowToRaw(
    callArrow: CallArrowToken[S]
  ): Alg[Option[CallArrowRaw]] =
    for {
      raw <- callArrow.ability.fold(
        for {
          myabeArrowType <- N.readArrow(callArrow.funcName)
        } yield myabeArrowType
          .map(arrowType =>
            CallArrowRaw.func(
              funcName = callArrow.funcName.value,
              baseType = arrowType
            )
          )
      )(ab =>
        N.read(ab.asName, mustBeDefined = false).flatMap {
          case Some(at: AbilityType) =>
            callArrowFromAbility(ab.asName, at, callArrow.funcName).pure
          case _ =>
            T.getType(ab.value).flatMap {
              case Some(at: AbilityType) =>
                callArrowFromAbility(ab.asName, at, callArrow.funcName).pure
              case _ =>
                (A.getArrow(ab, callArrow.funcName), A.getServiceId(ab)).mapN {
                  case (Some(at), Right(sid)) =>
                    CallArrowRaw
                      .service(
                        abilityName = ab.value,
                        serviceId = sid,
                        funcName = callArrow.funcName.value,
                        baseType = at
                      )
                      .some
                  case (Some(at), Left(true)) =>
                    CallArrowRaw
                      .ability(
                        abilityName = ab.value,
                        funcName = callArrow.funcName.value,
                        baseType = at
                      )
                      .some
                  case _ => none
                }
            }
        }
      )
      result <- raw.flatTraverse(r =>
        val arr = r.baseType
        for {
          argsCheck <- T.checkArgumentsNumber(
            callArrow.funcName,
            arr.domain.length,
            callArrow.args.length
          )
          args <- Option
            .when(argsCheck)(callArrow.args zip arr.domain.toList)
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

object ValuesAlgebra {

  given [S[_], Alg[_]: Monad](using
    N: NamesAlgebra[S, Alg],
    T: TypesAlgebra[S, Alg],
    A: AbilitiesAlgebra[S, Alg],
    E: ErrorsAlgebra[S, Alg]
  ): ValuesAlgebra[S, Alg] =
    new ValuesAlgebra[S, Alg]
}
