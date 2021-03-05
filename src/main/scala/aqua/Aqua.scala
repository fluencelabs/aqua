package aqua

import aqua.context.{Abilities, AbilitiesResolve, ArgsAndVars, Arrows, Types, VarTypes}
import aqua.context.scope.ScopeWalker
import aqua.context.walker.Walker
import aqua.parser.Block
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.parse.{Parser => P, Parser0 => P0}
import aqua.parser.lift.Span
import cats.data.Validated.Valid
import shapeless.HNil

object Aqua {
  private val parser: P0[List[Block[Span.F, HNil]]] = Block.blocks[Span.F]

  val step1 =
    Walker
      .hnil[Span.F]
      .andThen(new ScopeWalker(_))
      .andThen(new Arrows.ExpDef(_))
      .andThen(new ArgsAndVars.ExpDef(_))
      .andThen(new Types.ExpDef(_))
      .andThen(new Abilities.ExpDef(_))
      .andThen(new AbilitiesResolve.ExpDef(_))

  val step2 =
    new VarTypes.Checker[Span.F, step1.Out, step1.Out](Walker.noopFrom(step1))

  def parse(input: String): ValidatedNel[AquaError, List[Block[Span.F, step2.Out]]] =
    Validated
      .fromEither(
        parser
          .parseAll(input)
          .left
          .map(pe => NonEmptyList.one[AquaError](SyntaxError(pe.failedAtOffset, pe.expected)))
      )
      .andThen { blocks =>
        step1.walkValidate(blocks).leftMap(_.map(_.toStringF).map(sv => WalkerError(sv._1, sv._2)))
      }
      .andThen { blocks =>
        step2.walkValidate(blocks).leftMap(_.map(_.toStringF).map(sv => WalkerError(sv._1, sv._2)))
      }
}
