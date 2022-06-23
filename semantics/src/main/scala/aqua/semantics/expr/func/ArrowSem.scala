package aqua.semantics.expr.func

import aqua.parser.expr.FuncExpr
import aqua.parser.expr.func.ArrowExpr
import aqua.parser.lexer.{Arg, DataTypeToken}
import aqua.raw.Raw
import aqua.raw.arrow.ArrowRaw
import aqua.raw.ops.*
import aqua.raw.value.{ValueRaw, VarRaw}
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.types.{ArrowType, ProductType, StreamType, Type}
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

  def after[Alg[_]: Monad](funcArrow: ArrowType, bodyGen: Raw)(implicit
    T: TypesAlgebra[S, Alg],
    N: NamesAlgebra[S, Alg],
    A: AbilitiesAlgebra[S, Alg]
  ): Alg[Raw] =
    A.endScope() *> {
      for {
        streams <- N.streamsDefinedWithinScope()
        retValues <- T.endArrowScope(expr.arrowTypeExpr)
        retDerivedFrom <- N.getDerivedFrom(expr.token, retValues)
      } yield {
        bodyGen match {
          case FuncOp(m) =>
            // TODO: wrap with local on...via...

            // These streams are returned as streams
            val retStreams: Map[String, Option[Type]] =
              (retValues zip funcArrow.codomain.toList).collect {
                case (VarRaw(n, StreamType(_)), StreamType(_)) => n -> None
                case (VarRaw(n, StreamType(_)), t) => n -> Some(t)
              }.toMap

            val builtStreams = retStreams.collect { case (n, Some(t)) =>
              n -> t
            }
            val escapingStreams = retStreams.collect { case (n, None) =>
              n
            }

            val localStreams = streams -- funcArrow.domain.labelledData.map(_._1) -- escapingStreams
            // Remove stream arguments, and values returned as streams

            println("retValues: " + retValues)
            println("retDerivedFrom: " + retDerivedFrom)
            println("localStreams: " + localStreams)

            val (body, retValuesFix) = localStreams.foldLeft((m, retDerivedFrom)) {
              case ((b, rs: List[(ValueRaw, List[ValueRaw])]), n) =>
                if (rs.exists(v => v._1.varNames(n) || v._2.exists(_.varNames(n))))
                  // with index because we can have multiple results that used one stream
                  val indexed = rs.zipWithIndex
                  RestrictionTag(n, isStream = true).wrap(
                    SeqTag.wrap(
                      b :: indexed.collect {
                        case ((vn, derived), idx)
                            if vn.varNames(n) || derived.exists(_.varNames(n)) =>
                          val can = CanonicalizeTag(
                            vn,
                            Call.Export(s"${vn.varNames.mkString("")}-fix-$idx", vn.`type`)
                          ).leaf
                          println("can: " + can)
                          can
                      }: _*
                    )
                  ) -> indexed.map {
                    case ((vn, derived), idx) if vn.varNames(n) || derived.exists(_.varNames(n)) =>
                      (VarRaw(s"${vn.varNames.mkString("")}-fix-$idx", vn.`type`), derived)
                    case (vm, _) =>
                      vm
                  }
                else RestrictionTag(n, isStream = true).wrap(b) -> rs
            }
            // Restrict all the local streams and canonicalize results that use streams

            println("retValuesFix: " + retValuesFix)

            ArrowRaw(funcArrow, retValuesFix.map(_._1), body)
          case m =>
            m
        }
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
