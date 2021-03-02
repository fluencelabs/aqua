package aqua.parser

import aqua.parser.lexer.Token._
import aqua.parser.lexer.{Ability, ArrowName, Value, Var}
import cats.data.NonEmptyList
import cats.parse.{Parser => P}
import aqua.parser.lexer.Value.`value`
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser._
import cats.{Comonad, Functor}
import cats.syntax.functor._
import shapeless.HNil

sealed trait FuncOp[F[_], L] extends Expression[F, L]
sealed trait InstrOp[F[_], L] extends FuncOp[F, L]

sealed trait ExecOp[F[_], L] extends InstrOp[F, L]
sealed trait CallOp[F[_], L] extends ExecOp[F, L]

case class FuncCall[F[_], L](arrow: ArrowName[F], args: List[Value[F]], context: L) extends CallOp[F, L]

case class AbilityFuncCall[F[_], L](ability: Ability[F], call: FuncCall[F, L], context: L) extends CallOp[F, L]

case class Extract[F[_], L](vr: Var[F], from: CallOp[F, L], context: L) extends ExecOp[F, L]

case class On[F[_], L](peer: Value[F], ops: NonEmptyList[ExecOp[F, L]], context: L) extends InstrOp[F, L]

case class Par[F[_], L](f: F[Unit], op: InstrOp[F, L], context: L) extends FuncOp[F, L]

// TODO: can't be in Par, can be in On
sealed trait AbilityResolve[F[_], L] extends ExecOp[F, L] {
  def ability: Ability[F]
}
case class AbilityId[F[_], L](ability: Ability[F], id: Value[F], context: L) extends AbilityResolve[F, L]

object FuncOp {

  def funcCall[F[_]: LiftParser: Comonad]: P[FuncCall[F, HNil]] =
    (ArrowName.an[F] ~ P.repSep0(`value`, `,`).between(`(`, `)`)).map {
      case (fnName, args) ⇒ FuncCall(fnName, args, HNil)
    }

  def abilityFuncCall[F[_]: LiftParser: Comonad]: P[AbilityFuncCall[F, HNil]] =
    ((Ability.ab[F] <* `.`) ~ funcCall).map {
      case (abName, fc) ⇒ AbilityFuncCall(abName, fc, HNil)
    }

  def callOp[F[_]: LiftParser: Comonad]: P[CallOp[F, HNil]] =
    P.oneOf(funcCall[F] :: abilityFuncCall[F] :: Nil)

  def extract[F[_]: LiftParser: Comonad]: P[Extract[F, HNil]] =
    ((Var.v <* `<-`) ~ callOp[F]).map {
      case (v, f) ⇒ Extract(v, f, HNil)
    }

  def abilityResolve[F[_]: LiftParser: Comonad]: P[AbilityResolve[F, HNil]] =
    ((Ability.ab <* ` `) ~ `value`).map {
      case (n, v) ⇒ AbilityId[F, HNil](n, v, HNil)
    }.widen[AbilityResolve[F, HNil]]

  // TODO can't be in Par, can be in On
  def execOp[F[_]: LiftParser: Comonad]: P[ExecOp[F, HNil]] =
    P.oneOf(
      callOp.backtrack
        :: abilityResolve.backtrack
        :: extract :: Nil
    )

  def startOn[F[_]: LiftParser: Comonad]: P[Value[F]] = `on` *> ` ` *> `value` <* ` `.? <* `:` <* ` \n+`

  def execOn[F[_]: LiftParser: Comonad]: P[On[F, HNil]] =
    (startOn ~ indented(execOp[F])).map {
      case (v, i) ⇒ On(v, i, HNil)
    }

  def instrOp[F[_]: LiftParser: Comonad]: P[InstrOp[F, HNil]] =
    P.oneOf(
      execOn.backtrack
        :: execOp :: Nil
    )

  def parOp[F[_]: LiftParser: Comonad]: P[Par[F, HNil]] =
    ((`par`.lift <* ` `) ~ instrOp[F]).map(pi => Par(pi._1, pi._2, HNil))

  def `funcop`[F[_]: LiftParser: Comonad]: P[FuncOp[F, HNil]] =
    P.oneOf(parOp.backtrack :: instrOp :: Nil)

  def body[F[_]: LiftParser: Comonad]: P[NonEmptyList[FuncOp[F, HNil]]] = indented(`funcop`)

  implicit def funcOpFunctor[F[_]]: Functor[FuncOp[F, *]] =
    new Functor[FuncOp[F, *]] {

      override def map[A, B](fa: FuncOp[F, A])(f: A => B): FuncOp[F, B] =
        fa match {
          case fc @ FuncCall(_, _, ctx) => fc.copy(context = f(ctx))
          case afc @ AbilityFuncCall(_, fc, ctx) => afc.copy(call = fc.copy(context = f(fc.context)), context = f(ctx))
          case e @ Extract(_, afc @ AbilityFuncCall(_, fc, actx), ctx) =>
            e.copy(from = afc.copy(call = fc.copy(context = f(fc.context)), context = f(actx)), context = f(ctx))
          case e @ Extract(_, fc @ FuncCall(_, _, fctx), ctx) =>
            e.copy(from = fc.copy(context = f(fctx)), context = f(ctx))
          case on @ On(_, ops, ctx) => on.copy(ops = ops.map(map(_)(f).asInstanceOf[ExecOp[F, B]]), context = f(ctx))
          case p @ Par(_, op, ctx) => p.copy(op = map(op)(f).asInstanceOf[InstrOp[F, B]], context = f(ctx))
          case aid @ AbilityId(_, _, ctx) => aid.copy(context = f(ctx))
        }
    }

}
