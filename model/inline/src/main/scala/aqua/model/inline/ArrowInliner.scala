package aqua.model.inline

import aqua.model
import aqua.model.inline.state.{Arrows, Exports, Mangler}
import aqua.model.*
import aqua.raw.ops.RawTag
import aqua.types.{ArrowType, BoxType, StreamType}
import aqua.raw.value.{ValueRaw, VarRaw}
import cats.Eval
import cats.data.{Chain, State}
import cats.syntax.bifunctor.*
import scribe.Logging

/**
 * Does [[RawTag.Tree]] -> [[OpModel.Tree]] transformation:
 * - Converts [[ValueRaw]] to [[ValueModel]] via [[TagInliner]]
 * - For [[aqua.raw.ops.CallArrowTag]]
 */
object ArrowInliner extends Logging {

  def callArrow[S: Exports: Arrows: Mangler](
    arrow: FuncArrow,
    call: CallModel
  ): State[S, OpModel.Tree] =
    callArrowRet(arrow, call).map(_._1)

  // Get streams that was declared outside of a function
  private def getOutsideStreamNames[S: Exports]: State[S, Set[String]] =
    Exports[S].exports
      .map(exports =>
        exports.collect { case (n, VarModel(_, StreamType(_), _)) =>
          n
        }.toSet
      )

  // push results to streams if they are exported to streams
  private def pushStreamResults[S: Mangler: Exports: Arrows](
    outsideStreamNames: Set[String],
    exportTo: List[CallModel.Export],
    results: List[ValueRaw],
    body: OpModel.Tree
  ): State[S, (List[OpModel.Tree], List[ValueModel])] =
    for {
      // Fix return values with exports collected in the body
      resolvedResult <- RawValueInliner.valueListToModel(results)
    } yield {
      // Fix the return values
      (exportTo zip resolvedResult).map {
        case (
              CallModel.Export(n, StreamType(_)),
              (res @ VarModel(_, StreamType(_), _), resDesugar)
            ) if !outsideStreamNames.contains(n) =>
          resDesugar.toList -> res
        case (CallModel.Export(exp, st @ StreamType(_)), (res, resDesugar)) =>
          // pass nested function results to a stream
          (resDesugar.toList :+ PushToStreamModel(
            res,
            CallModel.Export(exp, st)
          ).leaf) -> VarModel(
            exp,
            st,
            Chain.empty
          )
        case (_, (res, resDesugar)) =>
          resDesugar.toList -> res
      }.foldLeft[(List[OpModel.Tree], List[ValueModel])](
        (body :: Nil, Nil)
      ) { case ((ops, rets), (fo, r)) =>
        (fo ::: ops, r :: rets)
      }
    }

  // Apply a callable function, get its fully resolved body & optional value, if any
  private def inline[S: Mangler: Arrows: Exports](
    fn: FuncArrow,
    call: CallModel
  ): State[S, (OpModel.Tree, List[ValueModel])] =
    getOutsideStreamNames.flatMap { outsideDeclaredStreams =>
      // Function's internal variables will not be available outside, hence the scope
      Exports[S].scope(
        for {
          // Process renamings, prepare environment
          tr <- prelude[S](fn, call)
          (tree, results) = tr

          // Register captured values as available exports
          _ <- Exports[S].resolved(fn.capturedValues)
          _ <- Mangler[S].forbid(fn.capturedValues.keySet)

          // Now, substitute the arrows that were received as function arguments
          // Use the new op tree (args are replaced with values, names are unique & safe)
          callableFuncBodyNoTopology <- TagInliner.handleTree(tree, fn.funcName)
          callableFuncBody =
            fn.capturedTopology
              .fold[OpModel](SeqModel)(ApplyTopologyModel.apply)
              .wrap(callableFuncBodyNoTopology)

          opsAndRets <- pushStreamResults(
            outsideDeclaredStreams,
            call.exportTo,
            results,
            callableFuncBody
          )
          (ops, rets) = opsAndRets
        } yield SeqModel.wrap(ops.reverse: _*) -> rets.reverse
      )
    }

  // Get all arrows that is arguments from outer Arrows.
  // Purge and push captured arrows and arrows as arguments into state.
  // Grab all arrows that must be renamed.
  private def updateArrowsAndRenameArrowArgs[S: Mangler: Arrows: Exports](
    args: ArgsCall,
    func: FuncArrow
  ): State[S, Map[String, String]] = {
    for {
      // Arrow arguments: expected type is Arrow, given by-name
      argsToArrowsRaw <- Arrows[S].argsArrows(args)
      argsToArrowsShouldRename <- Mangler[S].findNewNames(
        argsToArrowsRaw.keySet
      )
      argsToArrows = argsToArrowsRaw.map { case (k, v) =>
        argsToArrowsShouldRename.getOrElse(k, k) -> v
      }
      returnedArrows = func.ret.collect { case VarRaw(name, ArrowType(_, _)) =>
        name
      }.toSet

      returnedArrowsShouldRename <- Mangler[S].findNewNames(returnedArrows)
      renamedCapturedArrows = func.capturedArrows.map { case (k, v) =>
        returnedArrowsShouldRename.getOrElse(k, k) -> v
      }

      // Going to resolve arrows: collect them all. Names should never collide: it's semantically checked
      _ <- Arrows[S].purge
      _ <- Arrows[S].resolved(renamedCapturedArrows ++ argsToArrows)
    } yield {
      argsToArrowsShouldRename ++ returnedArrowsShouldRename
    }
  }

  private def updateExportsAndRenameDataArgs[S: Mangler: Arrows: Exports](
    args: ArgsCall
  ): State[S, Map[String, String]] = {
    // DataType arguments
    val argsToDataRaw = args.dataArgs
    for {
      // Find all duplicates in arguments
      // we should not rename arguments that will be renamed by 'streamToRename'
      argsToDataShouldRename <- Mangler[S].findNewNames(
        argsToDataRaw.keySet
      )

      // Do not rename arguments if they just match external names
      argsToData = argsToDataRaw.map { case (k, v) =>
        argsToDataShouldRename.getOrElse(k, k) -> v
      }

      _ <- Exports[S].resolved(argsToData)
    } yield argsToDataShouldRename
  }

  // Rename all exports-to-stream for streams that passed as arguments
  private def renameStreams(
    tree: RawTag.Tree,
    args: ArgsCall
  ): RawTag.Tree = {
    // Stream arguments
    val streamArgs = args.streamArgs

    // collect arguments with stream type
    // to exclude it from resolving and rename it with a higher-level stream that passed by argument
    val streamsToRename = streamArgs.view.mapValues(_.name).toMap

    if (streamsToRename.isEmpty) tree
    else
      tree
        .map(_.mapValues(_.map {
          // if an argument is a BoxType (Array or Option), but we pass a stream,
          // change a type as stream to not miss `$` sign in air
          // @see ArrowInlinerSpec `pass stream to callback properly` test
          case v @ VarRaw(name, baseType: BoxType) if streamsToRename.contains(name) =>
            v.copy(baseType = StreamType(baseType.element))
          case v: VarRaw if streamsToRename.contains(v.name) =>
            v.copy(baseType = StreamType(v.baseType))
          case v => v
        }))
        .renameExports(streamsToRename)
  }

  /**
   * Prepare the state context for this function call
   *
   * @param fn
   * Function that will be called
   * @param call
   * Call object
   * @tparam S
   * State
   * @return
   * Tree with substituted values, list of return values prior to function calling/inlining
   */
  private def prelude[S: Mangler: Arrows: Exports](
    fn: FuncArrow,
    call: CallModel
  ): State[S, (RawTag.Tree, List[ValueRaw])] =
    for {
      // Collect all arguments: what names are used inside the function, what values are received
      args <- State.pure(ArgsCall(fn.arrowType.domain, call.args))

      // Update states and rename tags
      renamedArrows <- updateArrowsAndRenameArrowArgs(args, fn)
      argsToDataShouldRename <- updateExportsAndRenameDataArgs(args)
      allShouldRename = argsToDataShouldRename ++ renamedArrows
      // Rename all renamed arguments in the body
      treeRenamed = fn.body.rename(allShouldRename)
      treeStreamsRenamed = renameStreams(treeRenamed, args)

      // Function body on its own defines some values; collect their names
      // except stream arguments. They should be already renamed
      treeDefines =
        treeStreamsRenamed.definesVarNames.value --
          args.streamArgs.keySet --
          args.streamArgs.values.map(_.name) --
          call.exportTo.filter { exp =>
            exp.`type` match {
              case StreamType(_) => false
              case _ => true
            }
          }.map(_.name)

      // We have some names in scope (forbiddenNames), can't introduce them again; so find new names
      shouldRename <- Mangler[S].findNewNames(treeDefines).map(_ ++ allShouldRename)
      _ <- Mangler[S].forbid(treeDefines ++ shouldRename.values.toSet)

      // If there was a collision, rename exports and usages with new names
      tree = treeStreamsRenamed.rename(shouldRename)

      // Result could be renamed; take care about that
    } yield (tree, fn.ret.map(_.renameVars(shouldRename)))

  private[inline] def callArrowRet[S: Exports: Arrows: Mangler](
    arrow: FuncArrow,
    call: CallModel
  ): State[S, (OpModel.Tree, List[ValueModel])] =
    for {
      passArrows <- Arrows[S].pickArrows(call.arrowArgNames)

      av <- Arrows[S].scope(
        for {
          _ <- Arrows[S].resolved(passArrows)
          av <- ArrowInliner.inline(arrow, call)
          // find and get resolved arrows if we return them from the function
          returnedArrows = av._2.collect { case VarModel(name, ArrowType(_, _), _) =>
            name
          }
          arrowsToSave <- Arrows[S].pickArrows(returnedArrows.toSet)
        } yield av -> arrowsToSave
      )
      ((appliedOp, values), arrowsToSave) = av
      _ <- Arrows[S].resolved(arrowsToSave)
      _ <- Exports[S].resolved(call.exportTo.map(_.name).zip(values).toMap)
    } yield appliedOp -> values
}
