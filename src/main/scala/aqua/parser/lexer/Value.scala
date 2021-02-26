package aqua.parser.lexer

import aqua.parser.BasicType
import aqua.parser.lexer.Token._
import cats.parse.{Numbers, Parser ⇒ P}

sealed trait Value
case class VarLambda(name: String, lambda: Option[String] = None) extends Value
case class Literal(value: String, ts: List[BasicType]) extends Value

object Value {
  val notLambdaSymbols = Set(' ', ',', '\n', ')', ':')

  val varLambda: P[VarLambda] = (`name` ~ (`.` *> P.charsWhile(c ⇒ !notLambdaSymbols(c))).?).map {
    case (n, l) ⇒ VarLambda(n, l)
  }

  val bool: P[Literal] = P.oneOf(("true" :: "false" :: Nil).map(t ⇒ P.string(t).as(Literal(t, BasicType.bool))))

  val num: P[Literal] = (P.char('-').?.with1 ~ Numbers.nonNegativeIntString).map {
    case (Some(_), n) ⇒ Literal(s"-$n", BasicType.signed)
    case (None, n) ⇒ Literal(n, BasicType.number)
  }

  val float: P[Literal] =
    (P.char('-').?.with1 ~ (Numbers.nonNegativeIntString <* P.char('.')) ~ Numbers.nonNegativeIntString).string
      .map(Literal(_, BasicType.float))

  // TODO make more sophisticated escaping/unescaping
  val string: P[Literal] =
    (`"` *> P.charsWhile0(_ != '"') <* `"`).string
      .map(Literal(_, BasicType.string))

  val literal: P[Literal] = P.oneOf(bool :: float.backtrack :: num :: string :: Nil)

  val `value`: P[Value] = P.oneOf(literal.backtrack :: varLambda :: Nil)

}
