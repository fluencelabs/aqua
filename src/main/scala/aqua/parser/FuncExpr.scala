package aqua.parser

import aqua.parser.lexer.Token._
import aqua.parser.lexer.{Ability, Name, Value}
import cats.data.NonEmptyList
import cats.parse.{Parser => P}
import aqua.parser.lexer.Value.`value`
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser._
import cats.{Comonad, Functor}
import cats.syntax.functor._
import cats.syntax.comonad._
import shapeless.HNil

sealed trait FuncExpr[F[_], L] extends Expression[F, L]
sealed trait InstrExpr[F[_], L] extends FuncExpr[F, L]

sealed trait ExecExpr[F[_], L] extends InstrExpr[F, L]

sealed trait CallExpr[F[_], L] extends ExecExpr[F, L] {
  def arrow: Name[F]
  def args: List[Value[F]]
}

case class FuncCall[F[_], L](arrow: Name[F], args: List[Value[F]], context: L) extends CallExpr[F, L]

case class AbilityFuncCall[F[_], L](
  ability: Ability[F],
  funcName: Name[F],
  arrow: Name[F],
  args: List[Value[F]],
  context: L
) extends CallExpr[F, L]

case class Extract[F[_], L](vr: Name[F], from: CallExpr[F, L], context: L) extends ExecExpr[F, L]

case class On[F[_], L](peer: Value[F], ops: NonEmptyList[ExecExpr[F, L]], context: L) extends InstrExpr[F, L]

case class Par[F[_], L](f: F[Unit], op: InstrExpr[F, L], context: L) extends FuncExpr[F, L]

// TODO: can't be in Par, can be in On
sealed trait AbilityResolve[F[_], L] extends ExecExpr[F, L] {
  def ability: Ability[F]
}
case class AbilityId[F[_], L](ability: Ability[F], id: Value[F], context: L) extends AbilityResolve[F, L]

object FuncExpr {

  def funcCall[F[_]: LiftParser: Comonad]: P[FuncCall[F, HNil]] =
    (Name.p[F] ~ P.repSep0(`value`, `,`).between(`(`, `)`)).map {
      case (fnName, args) ⇒ FuncCall(fnName, args, HNil)
    }

  def abilityFuncCall[F[_]: LiftParser: Comonad]: P[AbilityFuncCall[F, HNil]] =
    ((Ability.ab[F] <* `.`) ~ funcCall).map {
      case (abName, fc) ⇒
        AbilityFuncCall(
          abName,
          fc.arrow,
          Name(fc.arrow.as(abName.name.extract + "." + fc.arrow.name.extract)),
          fc.args,
          HNil
        )
    }

  def callOp[F[_]: LiftParser: Comonad]: P[CallExpr[F, HNil]] =
    P.oneOf(funcCall[F] :: abilityFuncCall[F] :: Nil)

  def extract[F[_]: LiftParser: Comonad]: P[Extract[F, HNil]] =
    ((Name.p <* `<-`) ~ callOp[F]).map {
      case (v, f) ⇒ Extract(v, f, HNil)
    }

  def abilityResolve[F[_]: LiftParser: Comonad]: P[AbilityResolve[F, HNil]] =
    ((Ability.ab <* ` `) ~ `value`).map {
      case (n, v) ⇒ AbilityId[F, HNil](n, v, HNil)
    }.widen[AbilityResolve[F, HNil]]

  // TODO can't be in Par, can be in On
  def execOp[F[_]: LiftParser: Comonad]: P[ExecExpr[F, HNil]] =
    P.oneOf(
      callOp.backtrack
        :: abilityResolve.backtrack
        :: extract :: Nil
    )

  def startOn[F[_]: LiftParser: Comonad]: P[Value[F]] = `on` *> ` ` *> `value` <* ` `.? <* `:` <* ` \n+`

  def execOn[F[_]: LiftParser: Comonad](indent: String): P[On[F, HNil]] =
    (startOn ~ indented(_ => execOp[F], indent)).map {
      case (v, i) ⇒ On(v, i, HNil)
    }

  def instrOp[F[_]: LiftParser: Comonad](indent: String): P[InstrExpr[F, HNil]] =
    P.oneOf(
      execOn(indent).backtrack
        :: execOp :: Nil
    )

  def parOp[F[_]: LiftParser: Comonad](indent: String): P[Par[F, HNil]] =
    ((`par`.lift <* ` `) ~ instrOp[F](indent)).map(pi => Par(pi._1, pi._2, HNil))

  def `funcop`[F[_]: LiftParser: Comonad](indent: String): P[FuncExpr[F, HNil]] =
    P.oneOf(parOp(indent).backtrack :: instrOp(indent) :: Nil)

  def body[F[_]: LiftParser: Comonad]: P[NonEmptyList[FuncExpr[F, HNil]]] = indented(`funcop`(_), "")

  implicit def funcOpFunctor[F[_]]: Functor[FuncExpr[F, *]] =
    new Functor[FuncExpr[F, *]] {

      override def map[A, B](fa: FuncExpr[F, A])(f: A => B): FuncExpr[F, B] =
        fa match {
          case fc @ FuncCall(_, _, ctx) => fc.copy(context = f(ctx))
          case afc @ AbilityFuncCall(_, _, _, _, ctx) => afc.copy(context = f(ctx))
          case e @ Extract(_, afc @ AbilityFuncCall(_, _, _, _, actx), ctx) =>
            e.copy(from = afc.copy(context = f(actx)), context = f(ctx))
          case e @ Extract(_, fc @ FuncCall(_, _, fctx), ctx) =>
            e.copy(from = fc.copy(context = f(fctx)), context = f(ctx))
          case on @ On(_, ops, ctx) => on.copy(ops = ops.map(map(_)(f).asInstanceOf[ExecExpr[F, B]]), context = f(ctx))
          case p @ Par(_, op, ctx) => p.copy(op = map(op)(f).asInstanceOf[InstrExpr[F, B]], context = f(ctx))
          case aid @ AbilityId(_, _, ctx) => aid.copy(context = f(ctx))
        }
    }

}
