package aqua

import aqua.parser.{ArrowType, Block, DataType, Type}
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.parse.{Parser => P, Parser0 => P0}
import aqua.parser.lift.Span
import cats.syntax.validated._

object Aqua {
  import aqua.parser.lexer.Token._

  val `parser`: P0[List[Block[Span.F]]] = P.repSep0(Block.`block`[Span.F], ` \n*`)

  def parse(input: String): ValidatedNel[Error, List[Block[Span.F]]] =
    Validated
      .fromEither(
        `parser`
          .parseAll(input)
          .left
          .map(pe => NonEmptyList.one[Error](SyntaxError(pe.failedAtOffset, pe.expected)))
      )
      .andThen(blocks =>
        Names
          .foldVerify[Span.F](blocks.map(Names.blockNames(_)))
          .leftMap(_.map(sp => NamesError(sp._1, sp._2)))
          .map(_ => blocks)
      )
}
