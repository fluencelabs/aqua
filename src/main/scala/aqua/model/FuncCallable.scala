package aqua.model

import aqua.semantics.{ArrowType, DataType, Type}
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

  val getDataService: String = "getDataSrv"
  val callbackService: String = "callbackSrv"

  val respFuncName = "response"
  val relayVarName = "relay"

  val callbackId: ValueModel = LiteralModel("\"" + callbackService + "\"")

  val returnCallback: Option[FuncOp] = ret.map { case (dv, t) =>
    viaRelay(
      FuncOp.leaf(
        CallServiceTag(
          callbackId,
          respFuncName,
          Call(
            (dv, t) :: Nil,
            None
          )
        )
      )
    )
  }

  // TODO it's an overkill
  def initPeerCallable(name: String, arrowType: ArrowType): FuncCallable =
    FuncCallable(
      viaRelay(
        FuncOp.leaf(
          CallServiceTag(
            callbackId,
            name,
            Call(
              arrowType.args.zipWithIndex.map { case (t, i) =>
                VarModel(s"arg$i") -> t
              },
              arrowType.res.map(_ => "init_call_res")
            )
          )
        )
      ),
      arrowType.args.zipWithIndex.map {
        case (t: DataType, i) => s"arg$i" -> Left(t)
        case (t: ArrowType, i) => s"arg$i" -> Right(t)
      },
      arrowType.res.map(VarModel("init_call_res") -> _),
      Map.empty
    )

  // TODO rename
  def generateTsModel: FuncOp =
    FuncOp
      .node(
        SeqTag,
        Chain
          .fromSeq(
            args.collect { case (argName, Left(_)) =>
              getDataOp(argName)
            } :+ getDataOp(relayVarName)
          )
          .append(
            apply(
              generateTsCall,
              args.collect { case (argName, Right(arrowType)) =>
                argName -> initPeerCallable(argName, arrowType)
              }.toMap,
              args.collect { case (argName, Left(_)) =>
                argName
              }.foldLeft(Set(relayVarName))(_ + _)
            ).value._1
          ) ++ Chain.fromSeq(returnCallback.toSeq)
      )
      .resolveTopology()

  def generateTsCall: Call =
    Call(
      args.map { case (k, e) =>
        (VarModel(k), e.fold(identity, identity))
      },
      None
    )

  def getDataOp(name: String): FuncOp =
    FuncOp.leaf(
      CallServiceTag(
        LiteralModel("\"" + getDataService + "\""),
        name,
        Call(Nil, Some(name))
      )
    )

  def viaRelay(op: FuncOp): FuncOp =
    FuncOp.wrap(OnTag(InitPeerIdModel, VarModel(relayVarName) :: Nil), op)

}
