package aqua.parse

import aqua.parse.Token._
import cats.data.NonEmptyList
import cats.parse.{Parser ⇒ P}
import Value.`value`

sealed trait FuncOp
sealed trait InstrOp extends FuncOp

sealed trait ExecOp extends InstrOp
sealed trait CallOp extends ExecOp

case class FuncCall(name: String, args: List[Value]) extends CallOp
case class AbilityFuncCall(ability: String, call: FuncCall) extends CallOp
case class Extract(v: String, from: CallOp) extends ExecOp

case class On(peer: Value, ops: NonEmptyList[ExecOp]) extends InstrOp

case class Par(op: InstrOp) extends FuncOp

// TODO: can't be in Par, can be in On
sealed trait AbilityResolve extends ExecOp
case class AbilityId(ability: String, id: Value) extends AbilityResolve

object FuncOp {


  val funcCall: P[FuncCall] =
    (`name` ~ P.repSep0(`value`, `,`).between(`(`, `)`)).map{
      case (fnName, args) ⇒ FuncCall(fnName, args)
    }

  val abilityFuncCall: P[AbilityFuncCall] =
    ((`Name` <* `.`) ~ funcCall).map{
      case (abName, fc) ⇒ AbilityFuncCall(abName, fc)
    }

  val callOp: P[CallOp] = P.oneOf(funcCall :: abilityFuncCall :: Nil)

  val extract: P[Extract] = ((`name` <* `<-`) ~ callOp).map{
    case (v, f) ⇒ Extract(v, f)
  }

  val abilityResolve: P[AbilityResolve] = ((`Name` <* ` `) ~ `value`).map{
    case (n, v) ⇒ AbilityId(n, v)
  }

  // TODO can't be in Par, can be in On
  val execOp: P[ExecOp] = P.oneOf( callOp.backtrack :: abilityResolve.backtrack :: extract :: Nil)

  val startOn: P[Value] = `on` *> ` ` *> `value` <* ` `.? <* `:` <* ` \n*`

  val execOn: P[On] =
    (startOn ~ indented(execOp)).map{
      case (v, i) ⇒ On(v, i)
    }

  val instrOp: P[InstrOp] = P.oneOf( execOn.backtrack :: execOp :: Nil)

  val parOp: P[Par] =
    `par` *> ` ` *> instrOp.map(Par)

  val `funcop`: P[FuncOp] = P.oneOf( parOp.backtrack :: instrOp :: Nil)

  val body: P[NonEmptyList[FuncOp]] = indented(`funcop`)
}
