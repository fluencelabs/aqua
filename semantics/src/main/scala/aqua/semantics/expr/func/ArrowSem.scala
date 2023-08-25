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

import cats.Eval
import cats.data.{Chain, NonEmptyList}
import cats.free.{Cofree, Free}
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.foldable.*
import cats.syntax.bifunctor.*
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
        val (bodyRets, retVals) = retsAndArgs.mapWithIndex {
          case ((v @ VarRaw(_, StreamType(_)), StreamType(_)), _) =>
            (Chain.empty, v)
          // canonicalize and change return value
          case ((VarRaw(streamName, streamType @ StreamType(streamElement)), _), idx) =>
            val canonReturnVar = VarRaw(s"-$streamName-fix-$idx", CanonStreamType(streamElement))
            val returnVar = VarRaw(s"-$streamName-flat-$idx", ArrayType(streamElement))
            val body = Chain(
              CanonicalizeTag(
                VarRaw(streamName, streamType),
                Call.Export(canonReturnVar.name, canonReturnVar.`type`)
              ).leaf,
              FlattenTag(
                canonReturnVar,
                returnVar.name
              ).leaf
            )

            (body, returnVar)
          // assign and change return value for all `Apply*Raw`
          case ((v: ValueRaw.ApplyRaw, _), idx) =>
            val assignedReturnVar = VarRaw(s"-return-fix-$idx", v.`type`)
            val body = Chain.one(
              AssignmentTag(
                v,
                assignedReturnVar.name
              ).leaf
            )

            (body, assignedReturnVar)
          case ((v, _), _) => (Chain.empty, v)
        }.unzip.leftMap(_.combineAll)

        val bodyModified = SeqTag.wrap(
          bodyModel +: bodyRets
        )

        // wrap streams with restrictions
        val bodyWithRestrictions = localStreams.foldLeft(bodyModified) {
          case (bm, (streamName, streamType)) =>
            RestrictionTag(streamName, streamType).wrap(bm)
        }

        ArrowRaw(funcArrow, retVals, bodyWithRestrictions)
      case _ => Raw.error("Invalid arrow body")
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
