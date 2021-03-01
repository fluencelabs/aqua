package aqua.parser

import aqua.parser.lexer.Token._
import aqua.parser.lexer.{Ability, Token, Value, Var}
import cats.data.NonEmptyList
import cats.parse.{Parser => P}
import aqua.parser.lexer.Value.`value`
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser._
import cats.{Comonad, Functor}
import cats.syntax.functor._

sealed trait FuncOp[F[_]] extends Token[F]
sealed trait InstrOp[F[_]] extends FuncOp[F]

sealed trait ExecOp[F[_]] extends InstrOp[F]
sealed trait CallOp[F[_]] extends ExecOp[F]

case class FuncCall[F[_]](name: F[String], args: List[Value[F]]) extends CallOp[F] {
  override def as[T](v: T)(implicit F: Functor[F]): F[T] = name.as(v)
}

case class AbilityFuncCall[F[_]](ability: Ability[F], call: FuncCall[F]) extends CallOp[F] {
  override def as[T](v: T)(implicit F: Functor[F]): F[T] = ability.as(v)
}

case class Extract[F[_]](vr: Var[F], from: CallOp[F]) extends ExecOp[F] {
  override def as[T](v: T)(implicit F: Functor[F]): F[T] = vr.as(v)
}

case class On[F[_]](peer: Value[F], ops: NonEmptyList[ExecOp[F]]) extends InstrOp[F] {
  override def as[T](v: T)(implicit F: Functor[F]): F[T] = peer.as(v)
}

case class Par[F[_]](f: F[Unit], op: InstrOp[F]) extends FuncOp[F] {
  override def as[T](v: T)(implicit F: Functor[F]): F[T] = f.as(v)
}

// TODO: can't be in Par, can be in On
sealed trait AbilityResolve[F[_]] extends ExecOp[F] {
  def ability: Ability[F]
  override def as[T](v: T)(implicit F: Functor[F]): F[T] = ability.as(v)
}
case class AbilityId[F[_]](ability: Ability[F], id: Value[F]) extends AbilityResolve[F]

object FuncOp {

  def funcCall[F[_]: LiftParser: Comonad]: P[FuncCall[F]] =
    (`name`.lift ~ P.repSep0(`value`, `,`).between(`(`, `)`)).map {
      case (fnName, args) ⇒ FuncCall(fnName, args)
    }

  def abilityFuncCall[F[_]: LiftParser: Comonad]: P[AbilityFuncCall[F]] =
    ((Ability.ab[F] <* `.`) ~ funcCall).map {
      case (abName, fc) ⇒ AbilityFuncCall(abName, fc)
    }

  def callOp[F[_]: LiftParser: Comonad]: P[CallOp[F]] =
    P.oneOf(funcCall[F] :: abilityFuncCall[F] :: Nil)

  def extract[F[_]: LiftParser: Comonad]: P[Extract[F]] =
    ((Var.v <* `<-`) ~ callOp[F]).map {
      case (v, f) ⇒ Extract(v, f)
    }

  def abilityResolve[F[_]: LiftParser: Comonad]: P[AbilityResolve[F]] =
    ((Ability.ab <* ` `) ~ `value`).map {
      case (n, v) ⇒ AbilityId(n, v)
    }.widen[AbilityResolve[F]]

  // TODO can't be in Par, can be in On
  def execOp[F[_]: LiftParser: Comonad]: P[ExecOp[F]] =
    P.oneOf(
      callOp.backtrack
        :: abilityResolve.backtrack
        :: extract :: Nil
    )

  def startOn[F[_]: LiftParser: Comonad]: P[Value[F]] = `on` *> ` ` *> `value` <* ` `.? <* `:` <* ` \n*`

  def execOn[F[_]: LiftParser: Comonad]: P[On[F]] =
    (startOn ~ indented(execOp[F])).map {
      case (v, i) ⇒ On(v, i)
    }

  def instrOp[F[_]: LiftParser: Comonad]: P[InstrOp[F]] =
    P.oneOf(
      execOn.backtrack
        :: execOp :: Nil
    )

  def parOp[F[_]: LiftParser: Comonad]: P[Par[F]] =
    ((`par`.lift <* ` `) ~ instrOp[F]).map(pi => Par(pi._1, pi._2))

  def `funcop`[F[_]: LiftParser: Comonad]: P[FuncOp[F]] =
    P.oneOf(parOp.backtrack :: instrOp :: Nil)

  def body[F[_]: LiftParser: Comonad]: P[NonEmptyList[FuncOp[F]]] = indented(`funcop`)
}
