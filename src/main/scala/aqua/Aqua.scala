package aqua

import aqua.context.ArgsAndVars
import aqua.context.scope.ScopeWalker
import aqua.context.walker.Walker
import aqua.parser.Block
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.parse.{Parser => P, Parser0 => P0}
import aqua.parser.lift.Span
import cats.data.Validated.Valid
import shapeless.HNil

object Aqua {
  import aqua.parser.lexer.Token._

  private val parser: P0[List[Block[Span.F, HNil]]] = Block.blocks[Span.F]

  val walker = Walker.hnil[Span.F].andThen(new ScopeWalker(_)).andThen(new ArgsAndVars.ExpDef(_))

  def parse(input: String): ValidatedNel[AquaError, List[Block[Span.F, walker.Out]]] =
    Validated
      .fromEither(
        parser
          .parseAll(input)
          .left
          .map(pe => NonEmptyList.one[AquaError](SyntaxError(pe.failedAtOffset, pe.expected)))
      )
      .andThen { blocks =>
        walker.walkValidate(blocks).leftMap(_.map(_.toStringF).map(sv => WalkerError(sv._1, sv._2)))
      }
}
