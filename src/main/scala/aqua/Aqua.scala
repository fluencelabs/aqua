package aqua

import aqua.model.{DataAcc, Passer, ScopePasser}
import aqua.parser.Block
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.parse.{Parser => P, Parser0 => P0}
import aqua.parser.lift.Span
import cats.data.Validated.Valid
import shapeless.HNil

object Aqua {
  import aqua.parser.lexer.Token._

  private val parser: P0[List[Block[Span.F, HNil]]] = Block.blocks[Span.F]

  val passer = Passer.hnil[Span.F].andThen(new ScopePasser(_)).andThen(new DataAcc.Pass(_))

  def parse(input: String): ValidatedNel[AquaError, List[Block[Span.F, passer.Out]]] =
    Validated
      .fromEither(
        parser
          .parseAll(input)
          .left
          .map(pe => NonEmptyList.one[AquaError](SyntaxError(pe.failedAtOffset, pe.expected)))
      )
      .andThen { blocks =>
        passer.pass(blocks).leftMap(_.map(sv => NamesError(sv._1, sv._2)))
      }
}
