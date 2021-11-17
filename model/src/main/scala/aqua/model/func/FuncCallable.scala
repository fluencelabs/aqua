package aqua.model.func

import aqua.model.ValueModel.varName
import aqua.model.func.raw.*
import aqua.model.{Model, ValueModel, VarModel}
import aqua.types.*
import cats.Eval
import cats.data.Chain
import cats.free.Cofree
import scribe.Logging

// TODO docs for class and all args
case class FuncCallable(
  funcName: String,
  body: FuncOp,
  arrowType: ArrowType,
  ret: List[ValueModel],
  capturedArrows: Map[String, FuncCallable],
  capturedValues: Map[String, ValueModel]
) extends Model with Logging {

  lazy val args: List[(String, Type)] = arrowType.domain.toLabelledList()
  lazy val argNames: List[String] = args.map(_._1)

  def findNewNames(forbidden: Set[String], introduce: Set[String]): Map[String, String] =
    (forbidden intersect introduce).foldLeft(Map.empty[String, String]) { case (acc, name) =>
      acc + (name -> LazyList
        .from(0)
        .map(name + _)
        .dropWhile(n => forbidden(n) || introduce(n) || acc.contains(n))
        .head)
    }

  def isStream(vm: ValueModel): Boolean =
    vm.`type` match {
      case StreamType(_) => true
      case _ => false
    }

  def extractStreamArgs(args: Map[String, ValueModel]): Map[String, ValueModel] =
    args.filter(arg => isStream(arg._2))

  // Apply a callable function, get its fully resolved body & optional value, if any
  def resolve(
    call: Call,
    arrows: Map[String, FuncCallable],
    forbiddenNames: Set[String]
  ): Eval[(FuncOp, List[ValueModel])] = {

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
    val streamToRename = streamArgs.map { case (k, v) => (k, varName(v)) }.collect {
      case (k, Some(v)) => (k, v)
    }

    // Find all duplicates in arguments
    val argsShouldRename = findNewNames(forbiddenNames, (argsToDataRaw ++ argsToArrowsRaw).keySet)
      // we shoudln't rename arguments that will be renamed by 'streamToRename'
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
        case VarModel(streamNameWasAlreadySubstitutedAbove, _, _) =>
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
    val result: List[ValueModel] = ret.map(_.resolveWith(argsToData)).map {
      case v: VarModel if shouldRename.contains(v.name) => v.copy(shouldRename(v.name))
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
          .map[(Option[FuncOp], ValueModel)] {
            case (exp @ Call.Export(_, StreamType(_)), res) =>
              // pass nested function results to a stream
              Some(FuncOps.pushToStream(res, exp)) -> exp.model
            case (_, res) =>
              None -> res
          }
          .foldLeft[(List[FuncOp], List[ValueModel])]((FuncOp(callableFuncBody) :: Nil, Nil)) {
            case ((ops, rets), (Some(fo), r)) => (fo :: ops, r :: rets)
            case ((ops, rets), (_, r)) => (ops, r :: rets)
          }

        FuncOps.seq(ops.reverse: _*) -> rets.reverse
      }
  }

}
