package aqua.model.inline

import aqua.model.ValueModel
import aqua.model.inline.state.{Arrows, Counter, Exports, Mangler}
import aqua.raw.arrow.{ArgsCall, FuncArrow}
import cats.Eval
import scribe.{log, Logging}
import aqua.raw.ops.{AssignmentTag, Call, CallArrowTag, ClosureTag, FuncOp, FuncOps, RawTag, SeqTag}
import aqua.raw.value.{ValueRaw, VarRaw}
import aqua.types.*
import cats.Eval
import cats.data.Chain
import cats.free.Cofree
import cats.data.State

object ArrowInliner extends Logging {

  // TODO: return ValueModel – values are substituted and resolved on this stage
  // TODO: FuncOp is also not complete: it still has topology, but not arrow calls; how to show it? ResTop?
  // Apply a callable function, get its fully resolved body & optional value, if any
  def inline[S: Mangler: Arrows: Exports: Counter](
    fn: FuncArrow,
    call: Call
  ): State[S, (FuncOp, List[ValueRaw])] =
    // Function's internal variables will not be available outside, hence the scope
    Exports[S].scope(
      for {
        // Process renamings, prepare environment
        tr <- prelude[S](fn, call)
        (tree, result) = tr

        // Register captured values as available exports
        _ <- Exports[S].resolved(fn.capturedValues)

        // Now, substitute the arrows that were received as function arguments
        // Use the new op tree (args are replaced with values, names are unique & safe)
        callableFuncBody <- handleTree(tree)

        // Fix return values with exports collected in the body
        resolvedExports <- Exports[S].exports
        resolvedResult = result.map(_.resolveWith(resolvedExports))

        // Fix the return values
        (ops, rets) = (call.exportTo zip resolvedResult)
          .map[(Option[FuncOp], ValueRaw)] {
            case (exp @ Call.Export(_, StreamType(_)), res) =>
              // pass nested function results to a stream
              Some(FuncOps.pushToStream(res, exp)) -> exp.model
            case (_, res) =>
              None -> res
          }
          .foldLeft[(List[FuncOp], List[ValueRaw])]((FuncOp(callableFuncBody) :: Nil, Nil)) {
            case ((ops, rets), (Some(fo), r)) => (fo :: ops, r :: rets)
            case ((ops, rets), (_, r)) => (ops, r :: rets)
          }
      } yield FuncOps.seq(ops.reverse: _*) -> rets.reverse
    )

  /**
   * Prepare the state context for this function call
   *
   * @param fn
   *   Function that will be called
   * @param call
   *   Call object
   * @tparam S
   *   State
   * @return
   *   Tree with substituted values, list of return values prior to function calling/inlining
   */
  private def prelude[S: Mangler: Arrows](
    fn: FuncArrow,
    call: Call
  ): State[S, (FuncOp.Tree, List[ValueRaw])] =
    for {
      // Collect all arguments: what names are used inside the function, what values are received
      argsFull <- State.pure(ArgsCall(fn.arrowType.domain, call.args))

      // DataType arguments
      argsToDataRaw = argsFull.dataArgs

      // Arrow arguments: expected type is Arrow, given by-name
      argsToArrowsRaw <- Arrows[S].argsArrows(argsFull)

      // collect arguments with stream type
      // to exclude it from resolving and rename it with a higher-level stream that passed by argument
      // TODO: what if we have streams in lambda???
      streamToRename = argsFull.streamArgs.view.mapValues(_.name).toMap

      // Find all duplicates in arguments
      // we should not rename arguments that will be renamed by 'streamToRename'
      argsShouldRename <- Mangler[S].findNewNames(
        argsToDataRaw.keySet ++ argsToArrowsRaw.keySet -- streamToRename.keySet
      )

      argsToData = argsToDataRaw.map { case (k, v) => argsShouldRename.getOrElse(k, k) -> v }
      argsToArrows = argsToArrowsRaw.map { case (k, v) => argsShouldRename.getOrElse(k, k) -> v }

      // Going to resolve arrows: collect them all. Names should never collide: it's semantically checked
      _ <- Arrows[S].purge
      _ <- Arrows[S].resolved(fn.capturedArrows ++ argsToArrows)

      // Substitute arguments (referenced by name and optional lambda expressions) with values
      // Also rename all renamed arguments in the body
      treeWithValues =
        fn.body
          .rename(argsShouldRename)
          .resolveValues(argsToData)
          .rename(streamToRename)

      // Function body on its own defines some values; collect their names
      // except stream arguments. They should be already renamed
      treeDefines =
        treeWithValues.definesVarNames.value --
          argsFull.streamArgs.keySet --
          argsFull.streamArgs.values.map(_.name) --
          call.exportTo.filter { exp =>
            exp.`type` match {
              case StreamType(_) => false
              case _ => true
            }
          }.map(_.name)

      // We have some names in scope (forbiddenNames), can't introduce them again; so find new names
      shouldRename <- Mangler[S].findNewNames(treeDefines)
      _ <- Mangler[S].forbid(treeDefines ++ shouldRename.values.toSet)

      // If there was a collision, rename exports and usages with new names
      treeRenamed = treeWithValues.rename(shouldRename)

      // Result could be derived from arguments, or renamed; take care about that
      result: List[ValueRaw] = fn.ret.map(_.resolveWith(argsToData)).map {
        case v: VarRaw if shouldRename.contains(v.name) => v.copy(shouldRename(v.name))
        case v => v
      }
    } yield (treeRenamed.tree, result)

  private def handleTree[S: Exports: Counter: Mangler: Arrows](
    tree: FuncOp.Tree
  ): State[S, FuncOp.Tree] =
    FuncOp.traverseS(tree, handleTag(_))

  // resolve values of this tag with resolved exports, lift to Cofree as a leaf
  private def resolveLeaf[S: Exports](tag: RawTag): State[S, FuncOp.Tree] =
    Exports[S].exports.map(resolvedExports =>
      FuncOp.leaf(tag.mapValues(_.resolveWith(resolvedExports))).tree
    )

  private def callArrow[S: Exports: Counter: Arrows: Mangler](
    arrow: FuncArrow,
    call: Call
  ): State[S, FuncOp.Tree] =
    for {
      callResolved <- Exports[S].resolveCall(call)

      // HERE we take values and desugarize them!
      cd <- Sugar.desugarize(callResolved)
      (callDesugarized, maybePrelude) = cd

      passArrows <- Arrows[S].pickArrows(callResolved.arrowArgNames)
      noNames <- Mangler[S].getForbiddenNames

      av <- Arrows[S].scope(
        for {
          _ <- Arrows[S].resolved(passArrows)
          av <- ArrowInliner.inline(arrow, callDesugarized)
        } yield av
      )
      (appliedOp, value) = av

      // If smth needs to be added before this function tree, add it with SeqTag
      fullOp = maybePrelude.fold(appliedOp)(prelude =>
        FuncOp.node(SeqTag, Chain(prelude, appliedOp))
      )

      // Function defines new names inside its body – need to collect them
      // TODO: actually it's done and dropped – so keep and pass it instead
      // (maybe it's already done btw, maybe we don't need to do anything)
      newNames = fullOp.definesVarNames.value

      _ <- Counter[S].incr
      _ <- Mangler[S].forbid(newNames)
      _ <- Exports[S].resolved(call.exportTo.map(_.name).zip(value).toMap)

    } yield fullOp.tree

  private def handleTag[S: Exports: Counter: Arrows: Mangler](tag: RawTag): State[S, FuncOp.Tree] =
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

}
