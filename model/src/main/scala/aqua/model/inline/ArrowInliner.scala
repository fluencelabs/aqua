package aqua.model.inline

import aqua.model
import aqua.model.{CallModel, FuncArrow, OpModel, ValueModel, VarModel}
import aqua.model.inline.state.{Arrows, Counter, Exports, Mangler}
import aqua.raw.ops.FuncOp.Tree
import cats.Eval
import scribe.{log, Logging}
import aqua.raw.ops.{AssignmentTag, Call, CallArrowTag, ClosureTag, FuncOp, FuncOps, RawTag, SeqTag}
import aqua.raw.value.{ValueRaw, VarRaw}
import aqua.types.*
import cats.syntax.traverse.*
import cats.Eval
import cats.data.{Chain, State, StateT}
import cats.free.Cofree

object ArrowInliner extends Logging {

  // Apply a callable function, get its fully resolved body & optional value, if any
  private def inline[S: Mangler: Arrows: Exports: Counter](
    fn: FuncArrow,
    call: CallModel
  ): State[S, (OpModel.Tree, List[ValueModel])] =
    // Function's internal variables will not be available outside, hence the scope
    Exports[S].scope(
      for {
        // Process renamings, prepare environment
        tr <- prelude[S](fn, call)
        (tree, result) = tr

        // Register captured values as available exports
        _ <- Exports[S].resolved(fn.capturedValues)
        _ <- Mangler[S].forbid(fn.capturedValues.keySet)

        // Now, substitute the arrows that were received as function arguments
        // Use the new op tree (args are replaced with values, names are unique & safe)
        callableFuncBody <- handleTree(tree)

        // Fix return values with exports collected in the body
        resolvedResult <- Sugar.desugarize(result)

        // Fix the return values
        (ops, rets) = (call.exportTo zip resolvedResult)
          .map[(List[OpModel.Tree], ValueModel)] {
            case (Call.Export(exp, st @ StreamType(_)), (res, resDesugar)) =>
              // pass nested function results to a stream
              (resDesugar.toList :+ OpModel
                .pushToStream(res, exp)) -> VarModel(exp, st, Chain.empty)
            case (_, (res, resDesugar)) =>
              resDesugar.toList -> res
          }
          .foldLeft[(List[OpModel.Tree], List[ValueModel])](
            (callableFuncBody :: Nil, Nil)
          ) { case ((ops, rets), (fo, r)) =>
            (fo ::: ops, r :: rets)
          }
      } yield OpModel.seq(ops.reverse) -> rets.reverse
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
  private def prelude[S: Mangler: Arrows: Exports](
    fn: FuncArrow,
    call: CallModel
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
      _ <- Exports[S].resolved(argsToData)

      argsToArrows = argsToArrowsRaw.map { case (k, v) => argsShouldRename.getOrElse(k, k) -> v }

      // Going to resolve arrows: collect them all. Names should never collide: it's semantically checked
      _ <- Arrows[S].purge
      _ <- Arrows[S].resolved(fn.capturedArrows ++ argsToArrows)

      // Rename all renamed arguments in the body
      treeRenamed =
        fn.body
          .rename(argsShouldRename)
          .rename(streamToRename)

      // Function body on its own defines some values; collect their names
      // except stream arguments. They should be already renamed
      treeDefines =
        treeRenamed.definesVarNames.value --
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
      tree = treeRenamed.rename(shouldRename)

      // Result could be renamed; take care about that
    } yield (tree.tree, fn.ret.map(_.renameVars(shouldRename)))

  private def traverseS[S](cf: Tree, f: RawTag => State[S, OpModel.Tree]): State[S, OpModel.Tree] =
    for {
      headTree <- f(cf.head)
      tail <- StateT.liftF(cf.tail)
      tailTree <- tail.traverse(traverseS[S](_, f))
    } yield headTree.copy(tail = headTree.tail.map(_ ++ tailTree))

  private def handleTree[S: Exports: Counter: Mangler: Arrows](
    tree: FuncOp.Tree
  ): State[S, OpModel.Tree] =
    traverseS(tree, handleTag(_))

  def callArrow[S: Exports: Counter: Arrows: Mangler](
    arrow: FuncArrow,
    call: CallModel
  ): State[S, OpModel.Tree] =
    for {
      passArrows <- Arrows[S].pickArrows(call.arrowArgNames)

      av <- Arrows[S].scope(
        for {
          _ <- Arrows[S].resolved(passArrows)
          av <- ArrowInliner.inline(arrow, call)
        } yield av
      )
      (appliedOp, value) = av

      _ <- Counter[S].incr
      _ <- Exports[S].resolved(call.exportTo.map(_.name).zip(value).toMap)

    } yield appliedOp

  private def handleTag[S: Exports: Counter: Arrows: Mangler](tag: RawTag): State[S, OpModel.Tree] =
    for {
      resolvedArrows <- Arrows[S].arrows

      desugarized <- Sugar.desugarize(tag)
      dPrefix = desugarized.flatMap(_._2)
      dTag = desugarized.map(_._1)

      body <- dTag match {
        case CallArrow(fn, c) if resolvedArrows.contains(fn) =>
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
          resolveLeaf(dTag)

        case _ =>
          resolveLeaf(dTag)
      }
    } yield
    // If smth needs to be added before this function tree, add it with Seq
    dPrefix.fold(body)(p => OpModel.seq(p :: body :: Nil))

}
