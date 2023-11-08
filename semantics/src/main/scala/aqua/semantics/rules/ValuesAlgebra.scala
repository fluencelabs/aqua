package aqua.semantics.rules

import aqua.helpers.syntax.optiont.*
import aqua.parser.lexer.*
import aqua.parser.lexer.InfixToken.{BoolOp, CmpOp, EqOp, MathOp, Op as InfOp}
import aqua.parser.lexer.PrefixToken.Op as PrefOp
import aqua.raw.value.*
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.mangler.ManglerAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.report.ReportAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.types.*

import cats.Monad
import cats.data.{NonEmptyList, OptionT}
import cats.instances.list.*
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import scribe.Logging

class ValuesAlgebra[S[_], Alg[_]: Monad](using
  N: NamesAlgebra[S, Alg],
  T: TypesAlgebra[S, Alg],
  A: AbilitiesAlgebra[S, Alg],
  M: ManglerAlgebra[Alg],
  report: ReportAlgebra[S, Alg]
) extends Logging {

  private def reportNamedArgsDuplicates(
    args: NonEmptyList[NamedArg[S]]
  ): Alg[Unit] = args
    .groupBy(_.argName.value)
    .filter { case (_, group) =>
      group.size > 1
    }
    .toList
    .traverse_ { case (name, group) =>
      group.traverse_ { arg =>
        report.error(
          arg.argName,
          s"Duplicate argument `$name`"
        )
      }
    }

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
        (for {
          _ <- OptionT.liftF(
            reportNamedArgsDuplicates(op.args)
          )
          fields <- op.args.traverse(arg => OptionT(valueToRaw(arg.argValue)).map(arg -> _))
          prop <- OptionT(T.resolveCopy(op, rootType, fields))
        } yield prop).value
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
      case l @ LiteralToken(_, t) =>
        LiteralRaw(l.value, t).some.pure[Alg]

      case VarToken(name) =>
        N.read(name, mustBeDefined = false).flatMap {
          case Some(t) =>
            VarRaw(name.value, t).some.pure
          case None =>
            (for {
              t <- OptionT(
                T.getType(name.value)
              ).collect { case st: ServiceType => st }
                // A hack to report name error, better to refactor
                .flatTapNone(N.read(name))
              rename <- OptionT(
                A.getServiceRename(name.asTypeToken)
              )
            } yield VarRaw(rename, t)).value
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
        (for {
          resolvedType <- OptionT(T.resolveNamedType(typeName))
          // Report duplicate fields
          _ <- OptionT.liftF(
            reportNamedArgsDuplicates(fields)
          )
          fieldsGiven <- fields
            .traverse(arg => OptionT(valueToRaw(arg.argValue)).map(arg.argName.value -> _))
            .map(_.toNem) // Take only last value for a field
          fieldsGivenTypes = fieldsGiven.map(_.`type`)
          generated <- OptionT.fromOption(
            resolvedType match {
              case struct: StructType =>
                (
                  struct.copy(fields = fieldsGivenTypes),
                  MakeStructRaw(fieldsGiven, struct)
                ).some
              case ability: AbilityType =>
                (
                  ability.copy(fields = fieldsGivenTypes),
                  AbilityRaw(fieldsGiven, ability)
                ).some
            }
          )
          (genType, genData) = generated
          data <- OptionT.whenM(
            T.ensureTypeMatches(dvt, resolvedType, genType)
          )(genData.pure)
        } yield data).value

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
          empty <- ct.mode match {
            // empty stream is not a Nil, it is an unique variable, that will be used as a stream
            case CollectionToken.Mode.StreamMode =>
              for {
                streamName <- M.rename("stream-anon")
                value = VarRaw(streamName, StreamType(BottomType))
                _ <- N.defineInternal(value.name, value.`type`)
              } yield value

            case _ => ValueRaw.Nil.pure
          }
          raw = valuesRawChecked.map(raws =>
            NonEmptyList
              .fromList(raws)
              .fold(empty) { nonEmpty =>
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
                    right = rightRaw,
                    resultType = ScalarType.bool
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
                      right = rightRaw,
                      resultType = ScalarType.bool
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

                lazy val hasFloat = List(lType, rType).exists(
                  _ acceptsValueOf LiteralType.float
                )

                val bop = iop match {
                  case MathOp.Add => ApplyBinaryOpRaw.Op.Add
                  case MathOp.Sub => ApplyBinaryOpRaw.Op.Sub
                  case MathOp.Mul if hasFloat => ApplyBinaryOpRaw.Op.FMul
                  case MathOp.Mul => ApplyBinaryOpRaw.Op.Mul
                  case MathOp.Div => ApplyBinaryOpRaw.Op.Div
                  case MathOp.Rem => ApplyBinaryOpRaw.Op.Rem
                  case MathOp.Pow => ApplyBinaryOpRaw.Op.Pow
                  case CmpOp.Gt => ApplyBinaryOpRaw.Op.Gt
                  case CmpOp.Gte => ApplyBinaryOpRaw.Op.Gte
                  case CmpOp.Lt => ApplyBinaryOpRaw.Op.Lt
                  case CmpOp.Lte => ApplyBinaryOpRaw.Op.Lte
                }

                lazy val numbersTypeBounded: Alg[ScalarType | LiteralType] = {
                  val resType = ScalarType.resolveMathOpType(lType, rType)
                  report
                    .warning(
                      it,
                      s"Result type of ($lType ${it.op} $rType) is unknown, " +
                        s"using ${resType.`type`} instead"
                    )
                    .whenA(resType.overflow)
                    .as(resType.`type`)
                }

                // Expected type sets of left and right operands, result type
                val (leftExp, rightExp, resTypeM) = iop match {
                  case MathOp.Add | MathOp.Sub | MathOp.Div | MathOp.Rem =>
                    (ScalarType.integer, ScalarType.integer, numbersTypeBounded)
                  case MathOp.Pow =>
                    (ScalarType.integer, ScalarType.unsigned, numbersTypeBounded)
                  case MathOp.Mul if hasFloat =>
                    (ScalarType.float, ScalarType.float, ScalarType.i64.pure)
                  case MathOp.Mul =>
                    (ScalarType.integer, ScalarType.integer, numbersTypeBounded)
                  case CmpOp.Gt | CmpOp.Lt | CmpOp.Gte | CmpOp.Lte =>
                    (ScalarType.integer, ScalarType.integer, ScalarType.bool.pure)
                }

                for {
                  leftChecked <- T.ensureTypeOneOf(l, leftExp, lType)
                  rightChecked <- T.ensureTypeOneOf(r, rightExp, rType)
                  resType <- resTypeM
                } yield Option.when(
                  leftChecked.isDefined && rightChecked.isDefined
                )(
                  ApplyBinaryOpRaw(
                    op = bop,
                    left = leftRaw,
                    right = rightRaw,
                    resultType = resType
                  )
                )

            }
          case _ => None.pure[Alg]
        }
    }

  def valueToCall(v: ValueToken[S]): Alg[Option[(ValueRaw, ArrowType)]] =
    valueToRaw(v).flatMap(
      _.flatTraverse {
        case ca: CallArrowRaw => (ca, ca.baseType).some.pure[Alg]
        case apr @ ApplyPropertyRaw(_, IntoArrowRaw(_, arrowType, _)) =>
          (apr, arrowType).some.pure[Alg]
        // TODO: better error message (`raw` formatting)
        case raw => report.error(v, s"Expected arrow call, got $raw").as(none)
      }
    )

  private def valueToTypedRaw(v: ValueToken[S], expectedType: Type): Alg[Option[ValueRaw]] =
    OptionT(valueToRaw(v))
      .flatMap(raw =>
        OptionT.whenM(
          T.ensureTypeMatches(v, expectedType, raw.`type`)
        )(raw.pure)
      )
      .value

  def valueToStringRaw(v: ValueToken[S]): Alg[Option[ValueRaw]] =
    valueToTypedRaw(v, LiteralType.string)

  def ensureIsString(v: ValueToken[S]): Alg[Boolean] =
    valueToStringRaw(v).map(_.isDefined)

  private def abilityArrow(
    ab: Name[S],
    at: NamedType,
    funcName: Name[S]
  ): OptionT[Alg, CallArrowRaw] =
    OptionT
      .fromOption(
        at.arrows.get(funcName.value)
      )
      .map(arrowType =>
        CallArrowRaw.ability(
          ab.value,
          funcName.value,
          arrowType
        )
      )
      .flatTapNone(
        report.error(
          funcName,
          s"Function `${funcName.value}` is not defined " +
            s"in `${ab.value}` of type `${at.fullName}`, " +
            s"available functions: ${at.arrows.keys.mkString(", ")}"
        )
      )

  private def callArrowFromFunc(
    funcName: Name[S]
  ): OptionT[Alg, CallArrowRaw] =
    OptionT(
      N.readArrow(funcName)
    ).map(arrowType =>
      CallArrowRaw.func(
        funcName = funcName.value,
        baseType = arrowType
      )
    )

  private def callArrowFromAbility(
    ab: NamedTypeToken[S],
    funcName: Name[S]
  ): OptionT[Alg, CallArrowRaw] = {
    lazy val nameTypeFromAbility = OptionT(
      N.read(ab.asName, mustBeDefined = false)
    ).collect { case nt: (AbilityType | ServiceType) => ab.asName -> nt }

    lazy val nameTypeFromService = for {
      st <- OptionT(
        T.getType(ab.value)
      ).collect { case st: ServiceType => st }
      rename <- OptionT(
        A.getServiceRename(ab)
      )
      renamed = ab.asName.rename(rename)
    } yield renamed -> st

    lazy val nameType = nameTypeFromAbility orElse nameTypeFromService.widen

    lazy val fromArrow = OptionT(
      A.getArrow(ab, funcName)
    ).map(at =>
      CallArrowRaw
        .ability(
          abilityName = ab.value,
          funcName = funcName.value,
          baseType = at
        )
    )

    /**
     * If we have a name and a type, get function from ability.
     * Otherwise, get function from arrow.
     *
     * It is done like so to not report irrelevant errors.
     */
    nameType.flatTransformT {
      case Some((name, nt)) => abilityArrow(name, nt, funcName)
      case _ => fromArrow
    }
  }

  private def callArrowToRaw(
    callArrow: CallArrowToken[S]
  ): Alg[Option[CallArrowRaw]] =
    (for {
      raw <- callArrow.ability
        .fold(callArrowFromFunc(callArrow.funcName))(ab =>
          callArrowFromAbility(ab, callArrow.funcName)
        )
      domain = raw.baseType.domain
      _ <- OptionT.withFilterF(
        T.checkArgumentsNumber(
          callArrow.funcName,
          domain.length,
          callArrow.args.length
        )
      )
      args <- callArrow.args
        .zip(domain.toList)
        .traverse { case (tkn, tp) =>
          for {
            valueRaw <- OptionT(valueToRaw(tkn))
            _ <- OptionT.withFilterF(
              T.ensureTypeMatches(tkn, tp, valueRaw.`type`)
            )
          } yield valueRaw
        }
    } yield raw.copy(arguments = args)).value

}

object ValuesAlgebra {

  given [S[_], Alg[_]: Monad](using
    N: NamesAlgebra[S, Alg],
    T: TypesAlgebra[S, Alg],
    A: AbilitiesAlgebra[S, Alg],
    M: ManglerAlgebra[Alg],
    E: ReportAlgebra[S, Alg]
  ): ValuesAlgebra[S, Alg] =
    new ValuesAlgebra[S, Alg]
}
