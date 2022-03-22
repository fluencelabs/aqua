package aqua.semantics.rules

import aqua.parser.lexer.*
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
import cats.instances.list.*
import cats.data.NonEmptyList

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
          v match {
            case l: LiteralToken[S] => l
            case VarToken(n, lambda) => lambda.lastOption.getOrElse(n)
            case c: CollectionToken[S] =>
              c.values.head // TODO: actually it should point on the whole expression
            case ca: CallArrowToken[S] => ca.funcName
          },
          expected,
          vt
        )
      case None => false.pure[Alg]
    }

  def resolveType(v: ValueToken[S]): Alg[Option[Type]] =
    valueToRaw(v).map(_.map(_.`type`))

  private def resolveSingleLambda(rootType: Type, op: LambdaOp[S]): Alg[Option[LambdaRaw]] =
    op match {
      case op: IntoField[S] =>
        T.resolveField(rootType, op)
      case op: IntoIndex[S] =>
        op.idx
          .fold[Alg[Option[ValueRaw]]](Option(LiteralRaw.Zero).pure[Alg])(
            valueToRaw
          )
          .flatMap {
            case None => None.pure[Alg]
            case Some(vv) => T.resolveIndex(rootType, op, vv)
          }
    }

  def valueToRaw(v: ValueToken[S]): Alg[Option[ValueRaw]] =
    v match {
      case l: LiteralToken[S] => Some(LiteralRaw(l.value, l.ts)).pure[Alg]
      case VarToken(name, ops) =>
        N.read(name).flatMap {
          case Some(t) =>
            // Prepare lambda expression: take the last known type and the next op, add next op to accumulator
            ops
              .foldLeft[Alg[(Option[Type], Chain[LambdaRaw])]]((Some(t) -> Chain.empty).pure[Alg]) {
                case (acc, op) =>
                  acc.flatMap {
                    // Some(tt) means that the previous lambda op was resolved successfully
                    case (Some(tt), lamb) =>
                      // Resolve a single lambda
                      resolveSingleLambda(tt, op).map {
                        // Lambda op resolved, add it to accumulator and update the last known type
                        case Some(l) => (Some(l.`type`), lamb :+ l)
                        // Lambda op is not resolved, it's an error, stop iterations
                        case None => (None, Chain.empty)
                      }

                    // We have already errored, do nothing
                    case _ => (None, Chain.empty).pure[Alg]
                  }

              }
              .map {
                // Some(_) means no errors occured
                case (Some(_), lambda) if lambda.length == ops.length =>
                  Some(lambda.foldLeft[ValueRaw](VarRaw(name.value, t)) { case (v, l) =>
                    ApplyLambdaRaw(v, l)
                  })
                case _ => None
              }

          case None =>
            None.pure[Alg]
        }
      case ct @ CollectionToken(_, values) =>
        values.traverse(valueToRaw).map(_.toList.flatten).map(NonEmptyList.fromList).map {
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

//     TODO case it @ InfixToken(l, r, i) if i.op == "+" =>
//        callArrowToRaw(
//          CallArrowToken[S](
//            Some(Ability[S](it.as("math"))),
//            Name[S](it.as("add")),
//            l :: r :: Nil
//          )
//        )

    }

  def callArrowToRaw(ca: CallArrowToken[S]): Alg[Option[CallArrowRaw]] =
    ca.ability
      .fold(
        N.readArrow(ca.funcName)
          .map(
            _.map(
              CallArrowRaw(
                None,
                ca.funcName.value,
                Nil,
                _,
                None
              )
            )
          )
      )(ab =>
        (A.getArrow(ab, ca.funcName), A.getServiceId(ab)).mapN {
          case (Some(at), Right(sid)) =>
            // Service call, actually
            Some(
              CallArrowRaw(
                Some(ab.value),
                ca.funcName.value,
                Nil,
                at,
                Some(sid)
              )
            )
          case (Some(at), Left(true)) =>
            // Ability function call, actually
            Some(
              CallArrowRaw(
                Some(ab.value),
                ca.funcName.value,
                Nil,
                at,
                None
              )
            )
          case _ => None
        }
      )
      .flatMap {
        case Some(r) =>
          val arr = r.baseType
          T.checkArgumentsNumber(ca.funcName, arr.domain.length, ca.args.length).flatMap {
            case false => Option.empty[CallArrowRaw].pure[Alg]
            case true =>
              (ca.args zip arr.domain.toList).traverse { case (tkn, tp) =>
                valueToRaw(tkn).flatMap {
                  case Some(v) => T.ensureTypeMatches(tkn, tp, v.`type`).map(Option.when(_)(v))
                  case None => None.pure[Alg]
                }
              }.map(_.toList.flatten).map {
                case args: List[ValueRaw] if args.length == arr.domain.length =>
                  Some(r.copy(arguments = args))
                case _ => None
              }
          }

        case None => Option.empty[CallArrowRaw].pure[Alg]
      }

  def checkArguments(token: Token[S], arr: ArrowType, args: List[ValueToken[S]]): Alg[Boolean] =
    // TODO: do we really need to check this?
    T.checkArgumentsNumber(token, arr.domain.length, args.length).flatMap {
      case false => false.pure[Alg]
      case true =>
        args
          .map[Alg[Option[(Token[S], Type)]]](tkn => resolveType(tkn).map(_.map(t => tkn -> t)))
          .zip(arr.domain.toList)
          .foldLeft(
            true.pure[Alg]
          ) { case (f, (ft, t)) =>
            (
              f,
              ft.flatMap {
                case None =>
                  false.pure[Alg]
                case Some((tkn, valType)) =>
                  T.ensureTypeMatches(tkn, t, valType)
              }
            ).mapN(_ && _)
          }
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
