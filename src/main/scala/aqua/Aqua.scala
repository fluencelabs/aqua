package aqua

import aqua.parser.{ArrowType, Block, DataType, Type}
import cats.data.NonEmptyMap
import cats.parse.{Parser ⇒ P, Parser0 ⇒ P0}
import aqua.parser.lift.Span

case class Aqua(
  inputs: Map[String, DataType],
  callbacks: Map[String, ArrowType],
  types: Map[String, Type],
  services: Map[String, NonEmptyMap[String, ArrowType]]
)

object Aqua {
  import aqua.parser.lexer.Token._

  val `parser`: P0[List[Block[Span.F]]] = P.repSep0(Block.`block`[Span.F], ` \n*`)
}
