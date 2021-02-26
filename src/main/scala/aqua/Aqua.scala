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

  def parse(input: String): Either[Error, List[Block[Span.F]]] =
    `parser`
      .parseAll(input)
      .left
      .map[Error](pe => SyntaxError(pe.failedAtOffset, pe.expected))
      .flatMap(blocks =>
        Names
          .foldVerify[Span.F](blocks.map(Names.blockNames(_)))
          .fold(
            sp => Left(NamesError(sp._1, sp._2)),
            _ => Right(blocks)
          )
      )
}
