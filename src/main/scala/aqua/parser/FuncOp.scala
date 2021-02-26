package aqua.parser

import aqua.parser.lexer.Token._
import aqua.parser.lexer.Value
import cats.data.NonEmptyList
import cats.parse.{Parser ⇒ P}
import aqua.parser.lexer.Value.`value`
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser._
import cats.Functor
import cats.syntax.functor._

sealed trait FuncOp[F[_]]
sealed trait InstrOp[F[_]] extends FuncOp[F]

sealed trait ExecOp[F[_]] extends InstrOp[F]
sealed trait CallOp[F[_]] extends ExecOp[F]

case class FuncCall[F[_]](name: F[String], args: List[F[Value]]) extends CallOp[F]
case class AbilityFuncCall[F[_]](ability: F[String], call: F[FuncCall[F]]) extends CallOp[F]
case class Extract[F[_]](v: F[String], from: F[CallOp[F]]) extends ExecOp[F]

case class On[F[_]](peer: F[Value], ops: NonEmptyList[F[ExecOp[F]]]) extends InstrOp[F]

case class Par[F[_]](op: F[InstrOp[F]]) extends FuncOp[F]

// TODO: can't be in Par, can be in On
sealed trait AbilityResolve[F[_]] extends ExecOp[F]
case class AbilityId[F[_]](ability: F[String], id: F[Value]) extends AbilityResolve[F]

object FuncOp {

  def funcCall[F[_]: LiftParser]: P[F[FuncCall[F]]] =
    (`name`.lift ~ P.repSep0(`value`.lift, `,`).between(`(`, `)`)).map {
      case (fnName, args) ⇒ FuncCall(fnName, args)
    }.lift

  def abilityFuncCall[F[_]: LiftParser]: P[F[AbilityFuncCall[F]]] =
    ((`Name`.lift <* `.`) ~ funcCall).map {
      case (abName, fc) ⇒ AbilityFuncCall(abName, fc)
    }.lift

  def callOp[F[_]: LiftParser: Functor]: P[F[CallOp[F]]] =
    P.oneOf(funcCall[F].map(_.widen[CallOp[F]]) :: abilityFuncCall[F].map(_.widen[CallOp[F]]) :: Nil)

  def extract[F[_]: LiftParser: Functor]: P[F[Extract[F]]] =
    ((`name`.lift <* `<-`) ~ callOp[F]).map {
      case (v, f) ⇒ Extract(v, f)
    }.lift

  def abilityResolve[F[_]: LiftParser: Functor]: P[F[AbilityResolve[F]]] =
    ((`Name`.lift <* ` `) ~ `value`.lift).map {
      case (n, v) ⇒ AbilityId(n, v)
    }.widen[AbilityResolve[F]].lift

  // TODO can't be in Par, can be in On
  def execOp[F[_]: LiftParser: Functor]: P[F[ExecOp[F]]] =
    P.oneOf(
      callOp.map(_.widen[ExecOp[F]]).backtrack
        :: abilityResolve.map(_.widen[ExecOp[F]]).backtrack
        :: extract.map(_.widen[ExecOp[F]]) :: Nil
    )

  def startOn[F[_]: LiftParser]: P[F[Value]] = `on` *> ` ` *> `value`.lift <* ` `.? <* `:` <* ` \n*`

  def execOn[F[_]: LiftParser: Functor]: P[F[On[F]]] =
    (startOn ~ indented(execOp[F])).map {
      case (v, i) ⇒ On(v, i)
    }.lift

  def instrOp[F[_]: LiftParser: Functor]: P[F[InstrOp[F]]] =
    P.oneOf(
      execOn.map(_.widen[InstrOp[F]]).backtrack
        :: execOp.map(_.widen[InstrOp[F]]) :: Nil
    )

  def parOp[F[_]: LiftParser: Functor]: P[F[Par[F]]] =
    (`par` *> ` ` *> instrOp[F].map(Par(_))).lift

  def `funcop`[F[_]: LiftParser: Functor]: P[F[FuncOp[F]]] =
    P.oneOf(parOp.map(_.widen[FuncOp[F]]).backtrack :: instrOp.map(_.widen[FuncOp[F]]) :: Nil)

  def body[F[_]: LiftParser: Functor]: P[NonEmptyList[F[FuncOp[F]]]] = indented(`funcop`)
}
