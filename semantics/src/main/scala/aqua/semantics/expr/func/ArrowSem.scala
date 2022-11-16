package aqua.semantics.expr.func

import aqua.parser.expr.FuncExpr
import aqua.parser.expr.func.ArrowExpr
import aqua.parser.lexer.{Arg, DataTypeToken}
import aqua.raw.Raw
import aqua.raw.arrow.ArrowRaw
import aqua.raw.ops.{SeqTag, *}
import aqua.raw.value.{
  ApplyFunctorRaw,
  ApplyGateRaw,
  ApplyPropertyRaw,
  CallArrowRaw,
  CollectionRaw,
  ValueRaw,
  VarRaw
}
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.types.{ArrayType, ArrowType, CanonStreamType, ProductType, StreamType, Type}
import cats.data.{Chain, NonEmptyList}
import cats.free.{Cofree, Free}
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import cats.{Applicative, Monad}

class ArrowSem[S[_]](val expr: ArrowExpr[S]) extends AnyVal {

  import expr.arrowTypeExpr

  def before[Alg[_]: Monad](implicit
    T: TypesAlgebra[S, Alg],
    N: NamesAlgebra[S, Alg],
    A: AbilitiesAlgebra[S, Alg]
  ): Alg[ArrowType] =
    // Begin scope -- for mangling
    A.beginScope(arrowTypeExpr) *> N.beginScope(arrowTypeExpr) *> T
      .beginArrowScope(
        arrowTypeExpr
      )
      .flatMap((arrowType: ArrowType) =>
        // Create local variables
        expr.arrowTypeExpr.args
          .flatMap(_._1)
          .zip(
            arrowType.domain.toList
          )
          .traverse {
            case (argName, t: ArrowType) =>
              N.defineArrow(argName, t, isRoot = false)
            case (argName, t) =>
              N.define(argName, t)
          }
          .as(arrowType)
      )

  private def assignRaw(
    v: ValueRaw,
    idx: Int,
    body: RawTag.Tree,
    returnAcc: Chain[ValueRaw]
  ): (SeqTag.Tree, Chain[ValueRaw], Int) = {
    val assignedReturnVar = VarRaw(s"-return-fix-$idx", v.`type`)
    (
      SeqTag.wrap(
        body :: AssignmentTag(
          v,
          assignedReturnVar.name
        ).leaf :: Nil: _*
      ),
      returnAcc :+ assignedReturnVar,
      idx + 1
    )
  }

  def after[Alg[_]: Monad](funcArrow: ArrowType, bodyGen: Raw)(implicit
    T: TypesAlgebra[S, Alg],
    N: NamesAlgebra[S, Alg],
    A: AbilitiesAlgebra[S, Alg]
  ): Alg[Raw] =
    A.endScope() *> (
      N.streamsDefinedWithinScope(),
      T.endArrowScope(expr.arrowTypeExpr)
        .flatMap(retValues => N.getDerivedFrom(retValues.map(_.varNames)).map(retValues -> _))
    ).mapN {
      case (
            streamsInScope: Map[String, StreamType],
            (retValues: List[ValueRaw], retValuesDerivedFrom: List[Set[String]])
          ) =>
        bodyGen match {
          case FuncOp(bodyModel) =>
            // TODO: wrap with local on...via...

            // These streams are returned as streams
            val retStreams: Map[String, Option[Type]] =
              (retValues zip funcArrow.codomain.toList).collect {
                case (VarRaw(n, StreamType(_)), StreamType(_)) => n -> None
                case (VarRaw(n, StreamType(_)), t) => n -> Some(t)
              }.toMap

            val streamsThatReturnAsStreams = retStreams.collect { case (n, None) =>
              n
            }.toSet

            val streamArguments = funcArrow.domain.labelledData.map(_._1)

            // Remove stream arguments, and values returned as streams
            val localStreams = streamsInScope -- streamArguments -- streamsThatReturnAsStreams

            // process stream that returns as not streams and all Apply*Raw
            val (bodyModified, returnValuesModified, _) = (retValues zip funcArrow.codomain.toList)
              .foldLeft[(RawTag.Tree, Chain[ValueRaw], Int)]((bodyModel, Chain.empty, 0)) {
                case ((bodyAcc, returnAcc, idx), rets) =>
                  rets match {
                    // do nothing
                    case (v @ VarRaw(_, StreamType(_)), StreamType(_)) =>
                      (bodyAcc, returnAcc :+ v, idx)
                    // canonicalize and change return value
                    case (VarRaw(streamName, streamType @ StreamType(streamElement)), _) =>
                      val canonReturnVar =
                        VarRaw(s"-$streamName-fix-$idx", CanonStreamType(streamElement))

                      val returnVar =
                        VarRaw(s"-$streamName-flat-$idx", ArrayType(streamElement))

                      (
                        SeqTag.wrap(
                          bodyAcc :: CanonicalizeTag(
                            VarRaw(streamName, streamType),
                            Call.Export(canonReturnVar.name, canonReturnVar.`type`)
                          ).leaf :: FlattenTag(
                            canonReturnVar,
                            returnVar.name
                          ).leaf :: Nil: _*
                        ),
                        returnAcc :+ returnVar,
                        idx + 1
                      )
                    // assign and change return value for all `Apply*Raw`
                    case (v: ApplyFunctorRaw, _) => assignRaw(v, idx, bodyAcc, returnAcc)
                    case (v: ApplyGateRaw, _) => assignRaw(v, idx, bodyAcc, returnAcc)
                    case (v: ApplyPropertyRaw, _) => assignRaw(v, idx, bodyAcc, returnAcc)
                    case (v: CallArrowRaw, _) => assignRaw(v, idx, bodyAcc, returnAcc)
                    case (v: CollectionRaw, _) => assignRaw(v, idx, bodyAcc, returnAcc)

                    case (v, _) => (bodyAcc, returnAcc :+ v, idx)
                  }

              }

            // wrap streams with restrictions
            val bodyWithRestrictions = localStreams.foldLeft(bodyModified) {
              case (bm, (streamName, _)) => RestrictionTag(streamName, isStream = true).wrap(bm)
            }

            ArrowRaw(funcArrow, returnValuesModified.toList, bodyWithRestrictions)
          case bodyModel =>
            bodyModel
        }
    } <* N.endScope()

  def program[Alg[_]: Monad](implicit
    T: TypesAlgebra[S, Alg],
    N: NamesAlgebra[S, Alg],
    A: AbilitiesAlgebra[S, Alg]
  ): Prog[Alg, Raw] =
    Prog.around(
      before[Alg],
      after[Alg]
    )

}
