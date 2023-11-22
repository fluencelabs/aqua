package aqua.semantics.expr.func

import aqua.parser.expr.func.ArrowExpr
import aqua.raw.Raw
import aqua.raw.arrow.ArrowRaw
import aqua.raw.ops.*
import aqua.raw.value.*
import aqua.semantics.Prog
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.locations.LocationsAlgebra
import aqua.semantics.rules.mangler.ManglerAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.types.*

import cats.Monad
import cats.data.Chain
import cats.free.Cofree
import cats.syntax.applicative.*
import cats.syntax.bifunctor.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.traverse.*

class ArrowSem[S[_]](val expr: ArrowExpr[S]) extends AnyVal {

  import expr.arrowTypeExpr

  def before[Alg[_]: Monad](implicit
    T: TypesAlgebra[S, Alg],
    N: NamesAlgebra[S, Alg]
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
    M: ManglerAlgebra[Alg]
  ): Alg[Raw] = for {
    streamsInScope <- N.streamsDefinedWithinScope()
    retValues <- T.endArrowScope(expr.arrowTypeExpr)
    allNames <- N.getAllNames()
    // TODO: wrap with local on...via...
    retsAndArgs = retValues zip funcArrow.codomain.toList
    streamsThatReturnAsStreams = retsAndArgs.collect {
      case (vr @ VarRaw(name, StreamType(_)), StreamType(_)) => (vr, name)
      case (vr @ StreamRaw(_, name, _), StreamType(_)) => (vr, name)
    }
    // streams that return as streams and derived to another variable
    derivedStreamRetValues <- N
      .getDerivedFrom(streamsThatReturnAsStreams.map(_._1.varNames))
      .map(_.flatten.toSet)

    res <- bodyGen match {
      case FuncOp(bodyModel) =>
        val streamArgNames = funcArrow.domain.labelledStreams.map { case (name, _) => name }

        println("all names: " + allNames)
        println("streams in scope: " + streamsInScope)
        println("streams arg names: " + streamArgNames)
        println("derived stream ret: " + derivedStreamRetValues)
        println("streamsThatReturnAsStreams: " + streamsThatReturnAsStreams)


        println("ret: " + retValues)

        // Remove arguments, and values returned as streams
        val localStreams = streamsInScope -- streamArgNames --
          streamsThatReturnAsStreams.map(_._2).toSet -- derivedStreamRetValues

        println("local streams: " + localStreams)

        // process stream that returns as not streams and all Apply*Raw
        retsAndArgs.traverse {
          case (v @ VarRaw(_, StreamType(_)), StreamType(_)) =>
            (Chain.empty, v).pure[Alg]
          // canonicalize and change return value
          case (VarRaw(streamName, streamType @ StreamType(streamElement)), _) =>
            for {
              canonName <- M.rename(s"-$streamName-fix")
              returnVarName <- M.rename(s"-$streamName-flat")
            } yield {
              val canonReturnVar = VarRaw(canonName, CanonStreamType(streamElement))
              val returnVar = VarRaw(returnVarName, ArrayType(streamElement))
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
            }

          // assign and change return value for all `Apply*Raw`
          case (v: ValueRaw.ApplyRaw, _) =>
            for {
              assignedReturnName <- M.rename(s"-return-fix")
            } yield {
              val assignedReturnVar = VarRaw(assignedReturnName, v.`type`)

              val body = Chain.one(
                AssignmentTag(
                  v,
                  assignedReturnVar.name
                ).leaf
              )

              (body, assignedReturnVar)
            }

          case (v, _) => (Chain.empty, v).pure[Alg]
        }.map(_.unzip.leftMap(_.combineAll)).map { case (bodyRets, retVals) =>
          val bodyModified = SeqTag.wrap(
            bodyModel +: bodyRets
          )

          // wrap streams with restrictions
          val bodyWithRestrictions =
            localStreams.foldLeft(bodyModified) { case (bm, (streamName, streamType)) =>
              RestrictionTag(streamName, streamType).wrap(bm)
            }
          ArrowRaw(funcArrow, retVals, bodyWithRestrictions)
        }

      case _ => Raw.error("Invalid arrow body").pure[Alg]
    }
  } yield res

  def program[Alg[_]: Monad](implicit
    T: TypesAlgebra[S, Alg],
    N: NamesAlgebra[S, Alg],
    A: AbilitiesAlgebra[S, Alg],
    L: LocationsAlgebra[S, Alg],
    M: ManglerAlgebra[Alg]
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
