package aqua.parse

import aqua.parse.Token._
import cats.data.NonEmptyList
import cats.parse.{Parser ⇒ P}

sealed trait FuncOp

case class SomeOp(data: String) extends FuncOp

object FuncOp {

  /*

func smth(a:PeerId, s:ServiceId, ret: O -> Unit):
  on a:
    e <- Service(s).call_smth()
  ret(e)


type Oracle:
  get_price: -> i32

func get_prices(how_many: i32, result: i32 -> () ) =
  on relay:
    oracles <- resolve[Oracle](how_many)
  on oracle <- oracles par:
    $p <- get_price
  on relay:
    avg_price <- int.average($p)

  result(avg_price)
   */

  val `funcop`: P[FuncOp] = P.charsWhile(_ != '\n').map(SomeOp)

  val body: P[NonEmptyList[FuncOp]] = indented(`funcop` <* ` \n`)
}
