package aqua.parse

import aqua.parse.Token._
import cats.parse.{Parser ⇒ P}
import cats.parse.Numbers

sealed trait Value
case class VarLambda(name: String, lambda: Option[String]) extends Value
case class Literal(value: String, ts: List[BasicType]) extends Value

object Value {
  val notLambdaSymbols = Set(' ', ',', '\n', ')', ':')

  val varLambda: P[VarLambda] = (`name` ~ (`.` *> P.charsWhile(c ⇒ !notLambdaSymbols(c))).?).map{
    case (n, l) ⇒ VarLambda(n, l)
  }

  val bool: P[Literal] = P.oneOf( ("true" :: "false" :: Nil).map(t ⇒  P.string(t).as(Literal(t, BasicType.bool)) ))

  val num: P[Literal] = (P.char('-').?.with1 ~ Numbers.nonNegativeIntString).map {
    case (Some(_), n) ⇒ Literal(s"-$n", BasicType.signed)
    case (None, n) ⇒ Literal(n, BasicType.number)
  }

  val float: P[Literal] =
    (P.char('-').?.with1 ~ (Numbers.nonNegativeIntString <* P.char('.')) ~ Numbers.nonNegativeIntString)
      .string
      .map(Literal(_, BasicType.float))

  val string: P[Literal] =
    (P.char('"') *> P.repUntil0(P.anyChar, !P.charWhere(_ != '\\') *> P.char('"'))).string.map(Literal(_, BasicType.string))

  val literal: P[Literal] = P.oneOf(bool :: float.backtrack :: num :: string :: Nil)

  val `value`: P[Value] = P.oneOf(literal.backtrack :: varLambda :: Nil)

}