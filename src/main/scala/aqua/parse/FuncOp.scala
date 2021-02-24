package aqua.parse

import aqua.parse.Token._
import cats.data.NonEmptyList
import cats.parse.{Parser ⇒ P}
import DataType.`customtypedef`

// TODO could be const
case class VarLambda(name: String, lambda: Option[String])

sealed trait FuncOp
sealed trait InstrOp extends FuncOp

sealed trait ExecOp extends InstrOp
sealed trait CallOp extends ExecOp

case class FuncCall(name: String, args: List[VarLambda]) extends CallOp
case class AbilityFuncCall(ability: CustomType, call: FuncCall) extends CallOp
case class Extract(v: String, from: CallOp) extends ExecOp

case class On(peer: VarLambda, ops: NonEmptyList[ExecOp]) extends InstrOp

case class Par(op: InstrOp) extends FuncOp

object FuncOp {

  val notLambdaSymbols = Set(' ', ',', '\n', ')', ':')

  val varLambda: P[VarLambda] = (`name` ~ (`.` *> P.charsWhile(c ⇒ !notLambdaSymbols(c))).?).map{
    case (n, l) ⇒ VarLambda(n, l)
  }

  val funcCall: P[FuncCall] =
    (`name` ~ P.repSep0(varLambda, `,`).between(`(`, `)`)).map{
      case (fnName, args) ⇒ FuncCall(fnName, args)
    }

  val abilityFuncCall: P[AbilityFuncCall] =
    ((`customtypedef` <* `.`) ~ funcCall).map{
      case (abName, fc) ⇒ AbilityFuncCall(abName, fc)
    }

  val callOp: P[CallOp] = P.oneOf(funcCall :: abilityFuncCall :: Nil)

  val extract: P[Extract] = ((`name` <* `<-`) ~ callOp).map{
    case (v, f) ⇒ Extract(v, f)
  }

  val execOp: P[ExecOp] = P.oneOf( callOp.backtrack :: extract :: Nil)

  val startOn: P[VarLambda] = `on` *> ` ` *> varLambda <* ` `.? <* `:` <* ` \n*`

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
