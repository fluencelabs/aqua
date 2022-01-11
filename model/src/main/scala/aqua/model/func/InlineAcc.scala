package aqua.model.func

import aqua.model.ValueModel
import aqua.raw.arrow.{FuncArrow, FuncRaw}
import aqua.raw.ops.{AssignmentTag, Call, CallArrowTag, ClosureTag, FuncOp, RawTag, SeqTag}
import aqua.raw.value.{ValueRaw, VarRaw}
import aqua.types.ArrowType
import scribe.Logging
import cats.data.State
import cats.data.Chain

case class InlineAcc(
                      noNames: Set[String] = Set.empty,
                      resolvedExports: Map[String, ValueRaw] = Map.empty,
                      resolvedArrows: Map[String, FuncArrow] = Map.empty,
                      instructionCounter: Int = 0
                    )

object InlineAcc extends Logging {

  // resolve values of this tag with resolved exports, lift to Cofree as a leaf
  private def resolveLeaf[S: Exports](tag: RawTag): State[S, FuncOp.Tree] =
    Exports[S].exports.map(resolvedExports =>
      FuncOp.leaf(tag.mapValues(_.resolveWith(resolvedExports))).tree
    )


  def unwrapValue[S: Counter](valueRaw: ValueRaw): State[S, Chain[FuncOp.Tree]] =
    for {
      _ <- Counter[S].incr
      // Take values from a chain
      // for each, recursiveRaw
      // group all into par
      // for each, recursiveRaw
      // if anything returned, put it into seq before this
    } yield ???

  private def desugarize[S](call: Call): State[S, (Call, Option[FuncOp])] =
    State.pure(call -> None)

  private def callArrow[S: Exports : Counter : Arrows : Mangler](
                                                                  arrow: FuncArrow,
                                                                  call: Call
                                                                ): State[S, FuncOp.Tree] =
    for {
      callResolved <- Exports[S].resolveCall(call)

      // HERE we should take values and desugarize them!
      cd <- desugarize(callResolved)
      (callDesugarized, maybePrelude) = cd

      passArrows <- Arrows[S].pickArrows(callResolved.arrowArgNames)
      noNames <- Mangler[S].getForbiddenNames

      av <- Arrows[S].scope(
        for {
          _ <- Arrows[S].resolved(passArrows)
          av <- ArrowInliner.inline(arrow, callResolved)
        } yield av
      )
      (appliedOp, value) = av

      fullOp = maybePrelude.fold(appliedOp)(prelude => FuncOp.node(SeqTag, Chain(prelude, appliedOp)))

      // Function defines new names inside its body – need to collect them
      // TODO: actually it's done and dropped – so keep and pass it instead
      newNames = fullOp.definesVarNames.value

      _ <- Counter[S].incr
      _ <- Mangler[S].forbid(newNames)
      _ <- Exports[S].resolved(call.exportTo.map(_.name).zip(value).toMap)

    } yield fullOp.tree

  def handleTag[S: Exports : Counter : Arrows : Mangler](tag: RawTag): State[S, FuncOp.Tree] =
    Arrows[S].arrows.flatMap(resolvedArrows =>
      tag match {
        case CallArrowTag(fn, c) if resolvedArrows.contains(fn) =>
          callArrow(resolvedArrows(fn), c)

        case ClosureTag(arrow) =>
          for {
            _ <- Arrows[S].resolved(arrow)
            tree <- resolveLeaf(tag)
          } yield tree

        case AssignmentTag(value, assignTo) =>
          for {
            _ <- Exports[S].resolved(assignTo, value)
            tree <- resolveLeaf(tag)
          } yield tree

        case CallArrowTag(fn, _) =>
          logger.error(
            s"UNRESOLVED arrow $fn, skipping, will become (null) in AIR! Known arrows: ${resolvedArrows.keySet}"
          )
          resolveLeaf(tag)

        case _ =>
          resolveLeaf(tag)
      }
    )


  given Counter[InlineAcc] =
    Counter.Simple.transformS(_.instructionCounter, (acc, i) => acc.copy(instructionCounter = i))

  given Mangler[InlineAcc] =
    Mangler.Simple.transformS(_.noNames, (acc, nn) => acc.copy(noNames = nn))

  given Arrows[InlineAcc] =
    Arrows.Simple.transformS(_.resolvedArrows, (acc, aa) => acc.copy(resolvedArrows = aa))

  given Exports[InlineAcc] =
    Exports.Simple.transformS(_.resolvedExports, (acc, ex) => acc.copy(resolvedExports = ex))

}
