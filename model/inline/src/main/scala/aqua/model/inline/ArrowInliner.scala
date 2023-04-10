package aqua.model.inline

import aqua.model.inline.state.{Arrows, Counter, Exports, Mangler}
import aqua.model.*
import aqua.raw.ops.RawTag
import aqua.types.ArrowType
import aqua.raw.value.{ValueRaw, VarRaw}
import aqua.types.{BoxType, StreamType}
import cats.data.{Chain, State, StateT}
import cats.syntax.traverse.*
import cats.syntax.show.*
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

  // Apply a callable function, get its fully resolved body & optional value, if any
  private def inline[S: Mangler: Arrows: Exports](
    fn: FuncArrow,
    call: CallModel
  ): State[S, (OpModel.Tree, List[ValueModel])] =
    Exports[S].exports
      .map(exports =>
        exports.collect { case e @ (_, VarModel(_, StreamType(_), _)) =>
          e
        }
      )
      .flatMap { outsideDeclaredStreams =>
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
            callableFuncBodyNoTopology <- TagInliner.handleTree(tree, fn.funcName)
            callableFuncBody =
              fn.capturedTopology
                .fold[OpModel](SeqModel)(ApplyTopologyModel.apply)
                .wrap(callableFuncBodyNoTopology)

            // Fix return values with exports collected in the body
            resolvedResult <- RawValueInliner.valueListToModel(result)
            // Fix the return values
            (ops, rets) = (call.exportTo zip resolvedResult)
              .map[(List[OpModel.Tree], ValueModel)] {
                case (
                      CallModel.Export(n, StreamType(_)),
                      (res @ VarModel(_, StreamType(_), _), resDesugar)
                    ) if !outsideDeclaredStreams.contains(n) =>
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
              }
              .foldLeft[(List[OpModel.Tree], List[ValueModel])](
                (callableFuncBody :: Nil, Nil)
              ) { case ((ops, rets), (fo, r)) =>
                (fo ::: ops, r :: rets)
              }
          } yield SeqModel.wrap(ops.reverse: _*) -> rets.reverse
        )
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
      argsFull <- State.pure(ArgsCall(fn.arrowType.domain, call.args))

      // DataType arguments
      argsToDataRaw = argsFull.dataArgs

      // Arrow arguments: expected type is Arrow, given by-name
      argsToArrowsRaw <- Arrows[S].argsArrows(argsFull)

      // collect arguments with stream type
      // to exclude it from resolving and rename it with a higher-level stream that passed by argument
      // TODO: what if we have streams in property???
      streamToRename = argsFull.streamArgs.view.mapValues(_.name).toMap

      // Find all duplicates in arguments
      // we should not rename arguments that will be renamed by 'streamToRename'
      argsShouldRename <- Mangler[S].findNewNames(
        argsToDataRaw.keySet ++ argsToArrowsRaw.keySet -- streamToRename.keySet
      )

      // Do not rename arguments if they just match external names
      argsToData = argsToDataRaw.map { case (k, v) =>
        argsShouldRename.getOrElse(k, k) -> v
      }
      _ <- Exports[S].resolved(argsToData)

      argsToArrows = argsToArrowsRaw.map { case (k, v) => argsShouldRename.getOrElse(k, k) -> v }

      // Going to resolve arrows: collect them all. Names should never collide: it's semantically checked
      _ <- Arrows[S].purge
      _ <- Arrows[S].resolved(fn.capturedArrows ++ argsToArrows)

      // Rename all renamed arguments in the body
      treeRenamed =
        fn.body
          .rename(argsShouldRename)
          .map(_.mapValues(_.map {
            // if an argument is a BoxType (Array or Option), but we pass a stream,
            // change a type as stream to not miss `$` sign in air
            // @see ArrowInlinerSpec `pass stream to callback properly` test
            case v @ VarRaw(name, baseType: BoxType) if streamToRename.contains(name) =>
              v.copy(baseType = StreamType(baseType.element))
            case v: VarRaw if streamToRename.contains(v.name) =>
              v.copy(baseType = StreamType(v.baseType))
            case v => v
          }))
          .renameExports(streamToRename)

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
      shouldRename <- Mangler[S].findNewNames(treeDefines).map(_ ++ argsShouldRename)
      _ <- Mangler[S].forbid(treeDefines ++ shouldRename.values.toSet)

      // If there was a collision, rename exports and usages with new names
      tree = treeRenamed.rename(shouldRename)

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
          // find and get resolved arrows if we return them
          returnedArrows = av._2.collect {
              case VarModel(name, ArrowType(_, _), _) => name
            }
          arrowsToSave <- Arrows[S].pickArrows(returnedArrows.toSet)
        } yield av -> arrowsToSave
      )
      ((appliedOp, values), arrowsToSave) = av
      _ <- Arrows[S].resolved(arrowsToSave)
      _ <- Exports[S].resolved(call.exportTo.map(_.name).zip(values).toMap)
    } yield appliedOp -> values

}
