package aqua.model.func

import aqua.model.func.body.{CallFunctionTag, FuncOp, OpTag}
import aqua.model.{ValueModel, VarModel}
import aqua.types.ArrowType
import cats.Eval
import cats.data.Chain
import cats.free.Cofree

case class FuncCallable(
  funcName: String,
  body: FuncOp,
  args: ArgsDef,
  ret: Option[Call.Arg],
  capturedArrows: Map[String, FuncCallable]
) {

  def arrowType: ArrowType =
    ArrowType(
      args.types,
      ret.map(_.`type`)
    )

  def findNewNames(forbidden: Set[String], introduce: Set[String]): Map[String, String] =
    (forbidden intersect introduce).foldLeft(Map.empty[String, String]) { case (acc, name) =>
      acc + (name -> LazyList
        .from(0)
        .map(name + _)
        .dropWhile(n => forbidden(n) || introduce(n) || acc.contains(n))
        .head)
    }

  // Apply a callable function, get its fully resolved body & optional value, if any
  def resolve(
    call: Call,
    arrows: Map[String, FuncCallable],
    forbiddenNames: Set[String]
  ): Eval[(FuncOp, Option[ValueModel])] = {

    // Collect all arguments: what names are used inside the function, what values are received
    val argsFull = args.call(call)
    // DataType arguments
    val argsToData = argsFull.dataArgs
    // Arrow arguments: expected type is Arrow, given by-name
    val argsToArrows = argsFull.arrowArgs(arrows)

    // Going to resolve arrows: collect them all. Names should never collide: it's semantically checked
    val allArrows = capturedArrows ++ argsToArrows

    // Substitute arguments (referenced by name and optional lambda expressions) with values
    val treeWithValues = body.resolveValues(argsToData)

    // Function body on its own defines some values; collect their names
    val treeDefines = treeWithValues.definesValueNames.value -- call.exportTo

    // We have some names in scope (forbiddenNames), can't introduce them again; so find new names
    val shouldRename = findNewNames(forbiddenNames, treeDefines)
    // If there was a collision, rename exports and usages with new names
    val treeRenamed =
      if (shouldRename.isEmpty) treeWithValues else treeWithValues.rename(shouldRename)

    // Result could be derived from arguments, or renamed; take care about that
    val result = ret.map(_.model).map(_.resolveWith(argsToData)).map {
      case v: VarModel if shouldRename.contains(v.name) => v.copy(shouldRename(v.name))
      case v => v
    }

    // Now, substitute the arrows that were received as function arguments
    FuncOp
      .traverseA(
        // Use the new op tree (args are replaced with values, names are unique & safe)
        treeRenamed.tree,
        // Accumulator: all used names are forbidden, if we set any more names -- forbid them as well
        (forbiddenNames ++ shouldRename.values ++ treeDefines) ->
          // Functions may export variables, so collect them
          Map.empty[String, ValueModel]
      ) {
        case ((noNames, resolvedExports), CallFunctionTag(fn, c)) if allArrows.contains(fn) =>
          // Apply arguments to a function – recursion
          val (appliedOp, value) =
            allArrows(fn)
              .resolve(c.mapValues(_.resolveWith(resolvedExports)), argsToArrows, noNames)
              .value

          // Function defines new names inside its body – need to collect them
          // TODO: actually it's done and dropped – so keep and pass it instead
          val newNames = appliedOp.definesValueNames.value
          // At the very end, will need to resolve what is used as results with the result values
          (noNames ++ newNames, resolvedExports ++ c.exportTo.zip(value)) -> appliedOp.tree
        case (acc @ (_, resolvedExports), tag) =>
          // All the other tags are already resolved and need no substitution
          acc -> Cofree[Chain, OpTag](
            tag.mapValues(_.resolveWith(resolvedExports)),
            Eval.now(Chain.empty)
          )
      }
      .map { case ((_, resolvedExports), b) =>
        // If return value is affected by any of internal functions, resolve it
        FuncOp(b) -> result.map(_.resolveWith(resolvedExports))
      }
  }

}
