package aqua.model

import aqua.model.body.{Call, CallArrowTag, FuncOp, OpTag}
import aqua.types.{ArrowType, DataType, Type}
import cats.Eval
import cats.data.Chain
import cats.free.Cofree

case class FuncCallable(
  body: FuncOp,
  args: List[(String, Either[DataType, ArrowType])],
  ret: Option[(ValueModel, Type)],
  capturedArrows: Map[String, FuncCallable]
) {

  def findNewNames(forbidden: Set[String], introduce: Set[String]): Map[String, String] =
    (forbidden intersect introduce).foldLeft(Map.empty[String, String]) { case (acc, name) =>
      acc + (name -> LazyList
        .from(0)
        .map(name + _)
        .dropWhile(n => forbidden(n) || introduce(n) || acc.contains(n))
        .head)
    }

  // Apply a callable function, get its fully resolved body & optional value, if any
  def apply(
    call: Call,
    arrows: Map[String, FuncCallable],
    forbiddenNames: Set[String]
  ): Eval[(FuncOp, Option[ValueModel])] = {
    // Collect all arguments: what names are used inside the function, what values are received
    val argsFull = args.zip(call.args)
    // DataType arguments
    val argsToData = argsFull.collect { case ((n, Left(_)), v) =>
      n -> v._1
    }.toMap
    // Arrow arguments: expected type is Arrow, given by-name
    val argsToArrows = argsFull.collect { case ((n, Right(_)), (VarModel(name, _), _)) =>
      n -> arrows(name)
    }.toMap

    // Going to resolve arrows: collect them all. Names should never collide: it's semantically checked
    val allArrows = capturedArrows ++ argsToArrows

    // Substitute arguments (referenced by name and optional lambda expressions) with values
    val treeWithValues = body.resolveValues(argsToData)

    // Function body on its own defines some values; collect their names
    val treeDefines = treeWithValues.definesValueNames.value

    // We have some names in scope (forbiddenNames), can't introduce them again; so find new names
    val shouldRename = findNewNames(forbiddenNames, treeDefines)
    // If there was a collision, rename exports and usages with new names
    val treeRenamed =
      if (shouldRename.isEmpty) treeWithValues else treeWithValues.rename(shouldRename)

    // Result could be derived from arguments, or renamed; take care about that
    val result = ret.map(_._1).map(_.resolveWith(argsToData)).map {
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
        case ((noNames, resolvedExports), CallArrowTag(None, fn, c)) if allArrows.contains(fn) =>
          // Apply arguments to a function – recursion
          val (appliedOp, value) =
            allArrows(fn)
              .apply(c.mapValues(_.resolveWith(resolvedExports)), argsToArrows, noNames)
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
