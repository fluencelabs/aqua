package aqua.model.func

import aqua.model.func.raw._
import aqua.model.{Model, ValueModel, VarModel}
import aqua.types.{ArrowType, StreamType, Type}
import cats.Eval
import cats.data.Chain
import cats.free.Cofree
import wvlet.log.Logger

case class FuncCallable(
  funcName: String,
  body: FuncOp,
  args: ArgsDef,
  ret: Option[(ValueModel, Type)],
  capturedArrows: Map[String, FuncCallable],
  capturedValues: Map[String, ValueModel]
) extends Model {

  private val logger = Logger.of[FuncCallable]
  import logger._

  def arrowType: ArrowType =
    ArrowType(
      args.types,
      ret.map(_._2)
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

    debug("Call: " + call)

    // Collect all arguments: what names are used inside the function, what values are received
    val argsFull = args.call(call)
    // DataType arguments
    val argsToDataRaw = argsFull.dataArgs
    // Arrow arguments: expected type is Arrow, given by-name
    val argsToArrowsRaw = argsFull.arrowArgs(arrows)

    // Find all duplicates in arguments
    val argsShouldRename = findNewNames(forbiddenNames, (argsToDataRaw ++ argsToArrowsRaw).keySet)
    val argsToData = argsToDataRaw.map { case (k, v) => argsShouldRename.getOrElse(k, k) -> v }
    val argsToArrows = argsToArrowsRaw.map { case (k, v) => argsShouldRename.getOrElse(k, k) -> v }

    // Going to resolve arrows: collect them all. Names should never collide: it's semantically checked
    val allArrows = capturedArrows ++ argsToArrows

    // Substitute arguments (referenced by name and optional lambda expressions) with values
    // Also rename all renamed arguments in the body
    val treeWithValues = body.rename(argsShouldRename).resolveValues(argsToData)

    // Function body on its own defines some values; collect their names
    val treeDefines = treeWithValues.definesVarNames.value

    // We have some names in scope (forbiddenNames), can't introduce them again; so find new names
    val shouldRename = findNewNames(forbiddenNames, treeDefines)
    // If there was a collision, rename exports and usages with new names
    val treeRenamed = treeWithValues.rename(shouldRename)
    val renamedExport =
      call.exportTo.map(e => e.copy(name = shouldRename.getOrElse(e.name, e.name)))

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
          capturedValues
      ) {
        case ((noNames, resolvedExports), tag @ AssignmentTag(value, assignTo)) =>
          (
            noNames,
            resolvedExports + (assignTo -> value.resolveWith(resolvedExports))
          ) -> Cofree[Chain, RawTag](
            tag.mapValues(_.resolveWith(resolvedExports)),
            Eval.now(Chain.empty)
          )

        case ((noNames, resolvedExports), CallArrowTag(fn, c)) if allArrows.contains(fn) =>
          // Apply arguments to a function – recursion
          val callResolved = c.mapValues(_.resolveWith(resolvedExports))
          val possibleArrowNames = callResolved.args.collect { case VarModel(m, _: ArrowType, _) =>
            m
          }.toSet

          val (appliedOp, value) =
            allArrows(fn)
              .resolve(callResolved, allArrows.view.filterKeys(possibleArrowNames).toMap, noNames)
              .value

          // Function defines new names inside its body – need to collect them
          // TODO: actually it's done and dropped – so keep and pass it instead
          val newNames = appliedOp.definesVarNames.value
          // At the very end, will need to resolve what is used as results with the result values
          (
            noNames ++ newNames,
            resolvedExports ++ c.exportTo.map(_.name).zip(value)
          ) -> appliedOp.tree
        case (acc @ (_, resolvedExports), tag) =>
          tag match {
            case CallArrowTag(fn, _) if !allArrows.contains(fn) =>
              error(s"UNRESOLVED $fn in $funcName, skipping, will become (null) in AIR!")
            case _ =>
          }

          // All the other tags are already resolved and need no substitution
          acc -> Cofree[Chain, RawTag](
            tag.mapValues(_.resolveWith(resolvedExports)),
            Eval.now(Chain.empty)
          )
      }
      .map { case ((_, resolvedExports), callableFuncBody) =>
        // If return value is affected by any of internal functions, resolve it
        (for {
          exp <- call.exportTo
          res <- result
          pair <- exp match {
            // TODO check variable, not string
            case Call.Export(name, StreamType(_)) if name != "-return-" =>
              // path nested function results to a stream
              Some(FuncOps.seq(FuncOp(callableFuncBody), FuncOps.identity(res, exp)) -> result)
            case Call.Export(_, StreamType(_)) =>
              // don't create identity for `-return-` stream
              Some(FuncOp(callableFuncBody) -> result)
            case _ => None
          }
        } yield {
          pair
        }).getOrElse(FuncOp(callableFuncBody) -> result.map(_.resolveWith(resolvedExports)))
      }
  }

}
