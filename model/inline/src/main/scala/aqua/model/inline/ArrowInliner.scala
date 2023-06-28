package aqua.model.inline

import aqua.model
import aqua.model.inline.state.{Arrows, Counter, Exports, Mangler}
import aqua.model.*
import aqua.raw.ops.RawTag
import aqua.types.{ArrowType, BoxType, ScopeType, StreamType}
import aqua.raw.value.{ValueRaw, VarRaw}
import cats.Eval
import cats.data.{Chain, IndexedStateT, State}
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

  private def getOutsideStreamNames[S: Exports](): State[S, Set[String]] =
    Exports[S].exports
      .map(exports =>
        exports.collect { case e @ (n, VarModel(_, StreamType(_), _)) =>
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
      val (ops, rets) = (exportTo zip resolvedResult)
        .map[(List[OpModel.Tree], ValueModel)] {
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
        }
        .foldLeft[(List[OpModel.Tree], List[ValueModel])](
          (body :: Nil, Nil)
        ) { case ((ops, rets), (fo, r)) =>
          (fo ::: ops, r :: rets)
        }

      (ops, rets)
    }

  // Apply a callable function, get its fully resolved body & optional value, if any
  private def inline[S: Mangler: Arrows: Exports](
    fn: FuncArrow,
    call: CallModel
  ): State[S, (OpModel.Tree, List[ValueModel])] =
    getOutsideStreamNames().flatMap { outsideDeclaredStreams =>
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

          _ <- pushStreamResults(outsideDeclaredStreams, call.exportTo, results, callableFuncBody)
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
      argsToDataRaw = argsFull.nonStreamDataArgs

      // Arrow arguments: expected type is Arrow, given by-name
      argsToArrowsRaw <- Arrows[S].argsArrows(argsFull)

      // collect arguments with stream type
      // to exclude it from resolving and rename it with a higher-level stream that passed by argument
      streamToRename = argsFull.streamArgs.view.mapValues(_.name).toMap

      // Find all duplicates in arguments
      // we should not rename arguments that will be renamed by 'streamToRename'
      argsShouldRename <- Mangler[S].findNewNames(
        argsToDataRaw.keySet ++ argsToArrowsRaw.keySet
      )

      // Do not rename arguments if they just match external names
      argsToData = argsToDataRaw.map { case (k, v) =>
        argsShouldRename.getOrElse(k, k) -> v
      }
      _ <- Exports[S].resolved(argsToData)

      argsToArrows = argsToArrowsRaw.map { case (k, v) => argsShouldRename.getOrElse(k, k) -> v }

      returnedArrows = fn.ret.collect { case VarRaw(name, ArrowType(_, _)) =>
        name
      }.toSet

      returnedArrowsShouldRename <- Mangler[S].findNewNames(returnedArrows)
      renamedCapturedArrows = fn.capturedArrows.map { case (k, v) =>
        returnedArrowsShouldRename.getOrElse(k, k) -> v
      }

      // Going to resolve arrows: collect them all. Names should never collide: it's semantically checked
      _ <- Arrows[S].purge
      _ <- Arrows[S].resolved(renamedCapturedArrows ++ argsToArrows)

      // Rename all renamed arguments in the body
      treeRenamed =
        fn.body
          .rename(argsShouldRename)
          .rename(returnedArrowsShouldRename)
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
    } yield (tree, fn.ret.map(_.renameVars(shouldRename ++ returnedArrowsShouldRename)))

  private def gatherAbilities[S: Exports: Arrows: Mangler](
    abilityVar: String,
    scopeType: ScopeType
  ): State[S, (Map[String, FuncArrow], Map[String, ValueModel])] = {
    for {
      exports <- Exports[S].exports
      arrows <- Arrows[S].arrows
      varsInAbility = scopeType.variables
      arrowsInAbility = scopeType.arrows
      absInAbility = scopeType.abilities
      arrsAndVarsList <- absInAbility.traverse { case (name, t) => gatherAbilities(name, t) }
    } yield {
      val arrsAndVars: (Map[String, FuncArrow], Map[String, ValueModel]) =
        arrsAndVarsList.fold((Map.empty, Map.empty)) { case (acc, (arrs, vars)) =>
          (acc._1 ++ arrs, acc._2 ++ vars)
        }
      val arrowsWithVars =
        arrowsInAbility.map(_._1).flatMap(n => exports.get(s"$abilityVar.$n")).flatMap {
          case vm @ VarModel(name, ArrowType(_, _), _) =>
            val fullName = s"$abilityVar.$name"
            arrows.get(fullName).map(arr => (fullName, arr, vm))
          case _ => None
        }
      val arrowsToStore = arrowsWithVars.map(el => el._1 -> el._2).toMap
      val vars = arrowsWithVars.map(el => el._1 -> el._3).toMap
      val varsToStore = varsInAbility
        .map(_._1)
        .flatMap { n =>
          val name = s"$abilityVar.$n"
          exports.get(name).map(name -> _)
        }
        .toMap ++ vars
      (arrowsToStore ++ arrsAndVars._1, varsToStore ++ arrsAndVars._2)
    }
  }

  // to gather abilities
  // get variables
  // store it to exprots
  // if variable is ArrowType
  // get arrows from Arrows and store it too
  private[inline] def callArrowRet[S: Exports: Arrows: Mangler](
    arrow: FuncArrow,
    call: CallModel
  ): State[S, (OpModel.Tree, List[ValueModel])] =
    for {
      arrows <- Arrows[S].arrows
      passArrows <- Arrows[S].pickArrows(call.arrowArgNames)
      exports <- Exports[S].exports
      _ = println(s"ARROWS IN CALL ARROW RET for ${arrow.funcName}: " + arrows.keys)
      _ = println(s"EXPORTS IN CALL ARROW RET for ${arrow.funcName}: " + exports.keys)
      arrsAndVars <- call.abilityArgs.traverse { case (a, b) => gatherAbilities(a, b) }
        .map(_.fold((Map.empty, Map.empty)) { case (acc, (arrs, vars)) =>
          (acc._1 ++ arrs, acc._2 ++ vars)
        })
      _ = println("ab args: " + call.abilityArgs)
      (arrs, vars) = arrsAndVars
      _ = println("arrs copied: " + arrs.keys)
      av <- Arrows[S].scope(
        for {
          _ <- Arrows[S].resolved(passArrows ++ arrs)
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
