package aqua.semantics.expr.func

import aqua.parser.expr.FuncExpr
import aqua.parser.expr.func.ArrowExpr
import aqua.parser.lexer.{Arg, DataTypeToken}
import aqua.raw.Raw
import aqua.raw.arrow.ArrowRaw
import aqua.raw.ops.{SeqTag, *}
import aqua.raw.value.*
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.locations.LocationsAlgebra
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
    A: AbilitiesAlgebra[S, Alg],
    L: LocationsAlgebra[S, Alg]
  ): Alg[ArrowType] = for {
    arrowType <- T.beginArrowScope(arrowTypeExpr)
    // Create local variables
    _ <- expr.arrowTypeExpr.args.flatMap { case (name, _) => name }
      .zip(arrowType.domain.toList)
      .traverse {
        case (argName, t: ArrowType) =>
          N.defineArrow(argName, t, isRoot = false)
        case (argName, t) =>
          N.define(argName, t)
      }
  } yield arrowType

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

  def after[Alg[_]: Monad](
    funcArrow: ArrowType,
    bodyGen: Raw
  )(using
    T: TypesAlgebra[S, Alg],
    N: NamesAlgebra[S, Alg],
    A: AbilitiesAlgebra[S, Alg],
    L: LocationsAlgebra[S, Alg]
  ): Alg[Raw] = for {
    streamsInScope <- N.streamsDefinedWithinScope()
    retValues <- T.endArrowScope(expr.arrowTypeExpr)
    retValuesDerivedFrom <- N.getDerivedFrom(retValues.map(_.varNames))
    res = bodyGen match {
      case FuncOp(bodyModel) =>
        // TODO: wrap with local on...via...

        val retsAndArgs = retValues zip funcArrow.codomain.toList

        val argNames = funcArrow.domain.labelledData.map { case (name, _) => name }
        val streamsThatReturnAsStreams = retsAndArgs.collect {
          case (VarRaw(n, StreamType(_)), StreamType(_)) => n
        }.toSet

        // Remove arguments, and values returned as streams
        val localStreams = streamsInScope -- argNames -- streamsThatReturnAsStreams

        // process stream that returns as not streams and all Apply*Raw
        val (bodyModified, returnValuesModified, _) = retsAndArgs
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
                      bodyAcc,
                      CanonicalizeTag(
                        VarRaw(streamName, streamType),
                        Call.Export(canonReturnVar.name, canonReturnVar.`type`)
                      ).leaf,
                      FlattenTag(
                        canonReturnVar,
                        returnVar.name
                      ).leaf
                    ),
                    returnAcc :+ returnVar,
                    idx + 1
                  )
                // assign and change return value for all `Apply*Raw`
                case (
                      v: (ApplyGateRaw | ApplyPropertyRaw | CallArrowRaw | CollectionRaw |
                        ApplyBinaryOpRaw | ApplyUnaryOpRaw),
                      _
                    ) =>
                  assignRaw(v, idx, bodyAcc, returnAcc)

                case (v, _) => (bodyAcc, returnAcc :+ v, idx)
              }

          }

        // wrap streams with restrictions
        val bodyWithRestrictions = localStreams.foldLeft(bodyModified) {
          case (bm, (streamName, streamType)) =>
            RestrictionTag(streamName, streamType).wrap(bm)
        }

        ArrowRaw(funcArrow, returnValuesModified.toList, bodyWithRestrictions)
      case bodyModel =>
        bodyModel
    }
  } yield res

  def program[Alg[_]: Monad](implicit
    T: TypesAlgebra[S, Alg],
    N: NamesAlgebra[S, Alg],
    A: AbilitiesAlgebra[S, Alg],
    L: LocationsAlgebra[S, Alg]
  ): Prog[Alg, Raw] =
    Prog
      .around(
        before[Alg],
        after[Alg]
      )
      .abilitiesScope(expr.arrowTypeExpr)
      .namesScope(expr.arrowTypeExpr)
      .locationsScope()

}
