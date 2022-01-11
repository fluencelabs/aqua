package aqua.model.func

import aqua.raw.arrow.{Func, FuncRaw}
import aqua.raw.ops.{AssignmentTag, Call, CallArrowTag, ClosureTag, FuncOp, RawTag}
import aqua.raw.value.{ValueRaw, VarRaw}
import aqua.types.ArrowType
import scribe.Logging

case class FuncApplyAcc(
  noNames: Set[String],
  resolvedExports: Map[String, ValueRaw],
  resolvedArrows: Map[String, Func],
  funcName: String,
  instructionCounter: Int = 0
) extends Logging {

  // resolve values of this tag with resolved exports
  //def resolve(tag: RawTag): RawTag = tag.mapValues(_.resolveWith(resolvedExports))

  // resolve values of this tag with resolved exports, lift to Cofree as a leaf
  def resolveLeaf(tag: RawTag): FuncOp.Tree =
    FuncOp.leaf(tag.mapValues(_.resolveWith(resolvedExports))).tree

  def incr: FuncApplyAcc = copy(instructionCounter = instructionCounter + 1)

  // Register the new export
  def withResolvedExport(exportName: String, value: ValueRaw): FuncApplyAcc =
    incr.copy(resolvedExports =
      resolvedExports + (exportName -> value.resolveWith(resolvedExports))
    )

  // Register the new arrow
  def withResolvedArrow(arrow: FuncRaw): FuncApplyAcc =
    incr.copy(resolvedArrows =
      resolvedArrows + (arrow.name -> arrow.capture(resolvedArrows, resolvedExports))
    )

  // Arrow call: resolve, register exports
  def callArrow(name: String, call: Call): (FuncApplyAcc, FuncOp.Tree) = {
    // Apply arguments to a function – recursion
    val callResolved = call.mapValues(_.resolveWith(resolvedExports))
    val possibleArrowNames = callResolved.args.collect { case VarRaw(m, _: ArrowType, _) =>
      m
    }.toSet

    val (appliedOp, value) =
      ArrowInliner
        .inline(
          resolvedArrows(name),
          callResolved,
          resolvedArrows.view.filterKeys(possibleArrowNames).toMap,
          noNames
        )
        .value

    // Function defines new names inside its body – need to collect them
    // TODO: actually it's done and dropped – so keep and pass it instead
    val newNames = appliedOp.definesVarNames.value
    // At the very end, will need to resolve what is used as results with the result values
    incr.copy(
      noNames ++ newNames,
      resolvedExports ++ call.exportTo.map(_.name).zip(value)
    ) -> appliedOp.tree
  }

  def handleTag(tag: RawTag): (FuncApplyAcc, FuncOp.Tree) =
    tag match {
      case CallArrowTag(fn, c) if resolvedArrows.contains(fn) =>
        callArrow(fn, c)
      case tag @ ClosureTag(arrow) =>
        withResolvedArrow(arrow) -> resolveLeaf(tag)
      case tag @ AssignmentTag(value, assignTo) =>
        withResolvedExport(assignTo, value) -> resolveLeaf(tag)
      case CallArrowTag(fn, _) =>
        logger.error(
          s"UNRESOLVED arrow $fn in $funcName, skipping, will become (null) in AIR!"
        )
        this -> resolveLeaf(tag)
      case _ =>
        // All the other tags are already resolved and need no substitution
        this -> resolveLeaf(tag)
    }

}
