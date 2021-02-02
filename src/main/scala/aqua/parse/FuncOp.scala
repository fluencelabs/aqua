package aqua.parse

import aqua.parse.Token._
import cats.data.NonEmptyList
import cats.parse.{Parser ⇒ P}

sealed trait FuncOp

case class SomeOp(data: String) extends FuncOp

object FuncOp {

  val `funcop`: P[FuncOp] = P.charsWhile(_ != '\n').map(SomeOp)

  val body: P[NonEmptyList[FuncOp]] = indented(`funcop` <* ` \n`)
}
