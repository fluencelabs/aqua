package aqua.model.func

import aqua.model.ValueModel
import aqua.raw.arrow.{ArgsCall, Func}
import cats.Eval
import scribe.Logging
import aqua.raw.ops.{Call, FuncOp, FuncOps}
import aqua.raw.value.{ValueRaw, VarRaw}
import aqua.types.*
import cats.Eval
import cats.data.Chain
import cats.free.Cofree

object ArrowInliner extends Logging {

  def findNewNames(forbidden: Set[String], introduce: Set[String]): Map[String, String] =
    (forbidden intersect introduce).foldLeft(Map.empty[String, String]) { case (acc, name) =>
      acc + (name -> LazyList
        .from(0)
        .map(name + _)
        .dropWhile(n => forbidden(n) || introduce(n) || acc.contains(n))
        .head)
    }

  def extractStreamArgs(args: Map[String, ValueRaw]): Map[String, ValueRaw] =
    args.filter(arg => isStream(arg._2))

  def isStream(vm: ValueRaw): Boolean =
    vm.`type` match {
      case StreamType(_) => true
      case _ => false
    }

  // TODO: return ValueModel – values are substituted and resolved on this stage
  // TODO: FuncOp is also not complete: it still has topology, but not arrow calls; how to show it? ResTop?
  // Apply a callable function, get its fully resolved body & optional value, if any
  def inline(
    fn: Func,
    call: Call,
    arrows: Map[String, Func],
    forbiddenNames: Set[String]
  ): Eval[(FuncOp, List[ValueRaw])] = {
    import fn._

    logger.debug("Call: " + call)

    // Collect all arguments: what names are used inside the function, what values are received
    val argsFull = ArgsCall(arrowType.domain, call.args)
    // DataType arguments
    val argsToDataRaw = argsFull.dataArgs
    // Arrow arguments: expected type is Arrow, given by-name
    val argsToArrowsRaw = argsFull.arrowArgs(arrows)

    // collect arguments with stream type
    // to exclude it from resolving and rename it with a higher-level stream that passed by argument
    val streamArgs = extractStreamArgs(argsToDataRaw)
    // TODO: what if we have streams in lambda???
    val streamToRename = streamArgs.collect { case (k, VarRaw(v, _, _)) => (k, v) }

    // Find all duplicates in arguments
    val argsShouldRename = findNewNames(forbiddenNames, (argsToDataRaw ++ argsToArrowsRaw).keySet)
      // we shoudl not rename arguments that will be renamed by 'streamToRename'
      .filter { case (k, _) => !streamToRename.contains(k) }
    val argsToData = argsToDataRaw.map { case (k, v) => argsShouldRename.getOrElse(k, k) -> v }
    val argsToArrows = argsToArrowsRaw.map { case (k, v) => argsShouldRename.getOrElse(k, k) -> v }

    // Going to resolve arrows: collect them all. Names should never collide: it's semantically checked
    val allArrows = capturedArrows ++ argsToArrows

    // Substitute arguments (referenced by name and optional lambda expressions) with values
    // Also rename all renamed arguments in the body
    val treeWithValues =
      body.rename(argsShouldRename).resolveValues(argsToData).rename(streamToRename)

    // Function body on its own defines some values; collect their names
    // except stream arguments. They should be already renamed
    val treeDefines =
      treeWithValues.definesVarNames.value -- streamArgs.keySet -- streamArgs.values.collect {
        case VarRaw(streamNameWasAlreadySubstitutedAbove, _, _) =>
          streamNameWasAlreadySubstitutedAbove
      } -- call.exportTo.filter { exp =>
        exp.`type` match {
          case StreamType(_) => false
          case _ => true
        }
      }.map(_.name)

    // We have some names in scope (forbiddenNames), can't introduce them again; so find new names
    val shouldRename = findNewNames(forbiddenNames, treeDefines)

    // If there was a collision, rename exports and usages with new names
    val treeRenamed = treeWithValues.rename(shouldRename)

    // Result could be derived from arguments, or renamed; take care about that
    val result: List[ValueRaw] = ret.map(_.resolveWith(argsToData)).map {
      case v: VarRaw if shouldRename.contains(v.name) => v.copy(shouldRename(v.name))
      case v => v
    }

    // Now, substitute the arrows that were received as function arguments
    FuncOp
      .traverseA(
        // Use the new op tree (args are replaced with values, names are unique & safe)
        treeRenamed.tree,
        // Accumulator: all used names are forbidden, if we set any more names -- forbid them as well
        FuncApplyAcc(
          forbiddenNames ++ shouldRename.values ++ treeDefines,
          // Functions may export variables, so collect them
          capturedValues,
          // They also can define arrows!
          allArrows,
          // Function name we're handling...
          funcName
        )
      )(_.handleTag(_))
      .map { case (acc, callableFuncBody) =>
        // If return value is affected by any of internal functions, resolve it
        val resolvedResult = result.map(_.resolveWith(acc.resolvedExports))

        val (ops, rets) = (call.exportTo zip resolvedResult)
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

        FuncOps.seq(ops.reverse: _*) -> rets.reverse
      }
  }

}
