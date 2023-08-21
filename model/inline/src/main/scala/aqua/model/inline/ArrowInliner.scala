package aqua.model.inline

import aqua.model
import aqua.model.*
import aqua.model.inline.state.{Arrows, Exports, Mangler}
import aqua.raw.ops.RawTag
import aqua.raw.value.{ValueRaw, VarRaw}
import aqua.types.{AbilityType, ArrowType, BoxType, StreamType, Type}

import cats.data.{Chain, IndexedStateT, State}
import cats.syntax.bifunctor.*
import cats.syntax.foldable.*
import cats.syntax.traverse.*
import cats.syntax.option.*
import cats.{Eval, Monoid}
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
      val (ops, rets) = (exportTo zip resolvedResult).map {
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

      (ops, rets)
    }

  /**
   * @param tree generated tree after inlining a function
   * @param returnedValues function return values
   * @param exportsToSave values that must be saved for future states
   * @param arrowsToSave arrows that must be saved for future states
   */
  case class InlineResult(
    tree: OpModel.Tree,
    returnedValues: List[ValueModel],
    exportsToSave: Map[String, ValueModel],
    arrowsToSave: Map[String, FuncArrow]
  )

  // Apply a callable function, get its fully resolved body & optional value, if any
  private def inline[S: Mangler: Arrows: Exports](
    fn: FuncArrow,
    call: CallModel,
    outsideDeclaredStreams: Set[String]
  ): State[S, InlineResult] =
    for {
      // Register captured values as available exports
      _ <- Exports[S].resolved(fn.capturedValues)
      _ <- Mangler[S].forbid(fn.capturedValues.keySet)

      // Now, substitute the arrows that were received as function arguments
      // Use the new op tree (args are replaced with values, names are unique & safe)
      callableFuncBodyNoTopology <- TagInliner.handleTree(fn.body, fn.funcName)
      callableFuncBody =
        fn.capturedTopology
          .fold[OpModel](SeqModel)(ApplyTopologyModel.apply)
          .wrap(callableFuncBodyNoTopology)

      opsAndRets <- pushStreamResults(
        outsideDeclaredStreams,
        call.exportTo,
        fn.ret,
        callableFuncBody
      )
      (ops, rets) = opsAndRets

      exports <- Exports[S].exports
      arrows <- Arrows[S].arrows
      // gather all arrows and variables from abilities
      returnedAbilities = rets.collect { case VarModel(name, at: AbilityType, _) => name -> at }
      varsFromAbilities = returnedAbilities.flatMap { case (name, at) =>
        getAbilityVars(name, None, at, exports)
      }.toMap
      arrowsFromAbilities = returnedAbilities.flatMap { case (name, at) =>
        getAbilityArrows(name, None, at, exports, arrows)
      }.toMap

      // find and get resolved arrows if we return them from the function
      returnedArrows = rets.collect { case VarModel(name, _: ArrowType, _) => name }.toSet
      arrowsToSave <- Arrows[S].pickArrows(returnedArrows)
    } yield InlineResult(
      SeqModel.wrap(ops.reverse),
      rets.reverse,
      varsFromAbilities,
      arrowsFromAbilities ++ arrowsToSave
    )

  /**
   * Get all arrows that is arguments from outer Arrows.
   * Purge and push captured arrows and arrows as arguments into state.
   * Grab all arrows that must be renamed.
   *
   * @param argsToArrowsRaw arguments with ArrowType
   * @param func function where captured and returned may exist
   * @param abilityArrows arrows from abilities that should be renamed
   * @return all arrows that must be renamed in function body
   */
  private def updateArrowsAndRenameArrowArgs[S: Mangler: Arrows: Exports](
    argsToArrowsRaw: Map[String, FuncArrow],
    func: FuncArrow,
    abilityArrows: Map[String, String]
  ): State[S, Map[String, String]] = {
    for {
      argsToArrowsShouldRename <- Mangler[S]
        .findNewNames(
          argsToArrowsRaw.keySet
        )
        .map(_ ++ abilityArrows)
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

      _ <- Arrows[S].resolved(renamedCapturedArrows ++ argsToArrows)
    } yield {
      argsToArrowsShouldRename ++ returnedArrowsShouldRename
    }
  }

  /**
   * @param argsToDataRaw data arguments to rename
   * @param abilityValues values from abilities to rename
   * @return all values that must be renamed in function body
   */
  private def updateExportsAndRenameDataArgs[S: Mangler: Arrows: Exports](
    argsToDataRaw: Map[String, ValueModel],
    abilityValues: Map[String, String]
  ): State[S, Map[String, String]] = {
    for {
      // Find all duplicates in arguments
      // we should not find new names for 'abilityValues' arguments that will be renamed by 'streamToRename'
      argsToDataShouldRename <- Mangler[S]
        .findNewNames(
          argsToDataRaw.keySet
        )
        .map(_ ++ abilityValues)

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
    streamArgs: Map[String, VarModel]
  ): RawTag.Tree = {
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

  case class AbilityResolvingResult(
    namesToRename: Map[String, String],
    renamedExports: Map[String, ValueModel],
    renamedArrows: Map[String, FuncArrow]
  )

  given Monoid[AbilityResolvingResult] with

    override val empty: AbilityResolvingResult =
      AbilityResolvingResult(Map.empty, Map.empty, Map.empty)

    override def combine(
      a: AbilityResolvingResult,
      b: AbilityResolvingResult
    ): AbilityResolvingResult =
      AbilityResolvingResult(
        a.namesToRename ++ b.namesToRename,
        a.renamedExports ++ b.renamedExports,
        a.renamedArrows ++ b.renamedArrows
      )

  /**
   * Generate new names for all ability fields and arrows if necessary.
   * Gather all fields and arrows from Arrows and Exports states
   * @param name ability name in state
   * @param vm ability variable
   * @param t ability type
   * @param exports previous Exports
   * @param arrows previous Arrows
   * @return names to rename, Exports and Arrows with all ability fields and arrows
   */
  private def renameAndResolveAbilities[S: Mangler: Arrows: Exports](
    name: String,
    vm: VarModel,
    t: AbilityType,
    exports: Map[String, ValueModel],
    arrows: Map[String, FuncArrow]
  ): State[S, AbilityResolvingResult] = {
    for {
      newName <- Mangler[S].findNewName(name)
      newFieldsName = t.fields.mapBoth { case (n, _) =>
        AbilityType.fullName(name, n) -> AbilityType.fullName(newName, n)
      }
      allNewNames = newFieldsName.add((name, newName)).toSortedMap
      allVars = getAbilityVars(vm.name, newName.some, t, exports)
      allArrows = getAbilityArrows(vm.name, newName.some, t, exports, arrows)
    } yield AbilityResolvingResult(allNewNames, allVars, allArrows)
  }

  private def getAbilityFields[T <: Type](
    abilityName: String,
    abilityNewName: Option[String],
    abilityType: AbilityType,
    exports: Map[String, ValueModel]
  )(fields: AbilityType => Map[String, T]): Map[String, ValueModel] =
    fields(abilityType).flatMap { case (fName, _) =>
      val fullName = AbilityType.fullName(abilityName, fName)
      val newFullName = AbilityType.fullName(abilityNewName.getOrElse(abilityName), fName)

      println(s"getAbilityFields: $fullName -> $newFullName, ${Exports
        .getLastValue(fullName, exports)}")

      Exports
        .getLastValue(fullName, exports)
        .map(newFullName -> _)
    }

  private def getAbilityVars(
    abilityName: String,
    abilityNewName: Option[String],
    abilityType: AbilityType,
    exports: Map[String, ValueModel]
  ): Map[String, ValueModel] = {
    val get = getAbilityFields(
      abilityName,
      abilityNewName,
      abilityType,
      exports
    )

    get(_.variables) ++ get(_.arrows).flatMap {
      case arrow @ (_, vm: VarModel) =>
        arrow.some
      case (_, m) =>
        logger.error(s"Unexpected: '$m' cannot be an arrow")
        None
    }
  }

  private def getAbilityArrows(
    abilityName: String,
    abilityNewName: Option[String],
    abilityType: AbilityType,
    exports: Map[String, ValueModel],
    arrows: Map[String, FuncArrow]
  ): Map[String, FuncArrow] = {
    val get = getAbilityFields(
      abilityName,
      abilityNewName,
      abilityType,
      exports
    )

    get(_.arrows).flatMap {
      case (_, VarModel(name, _, _)) =>
        arrows.get(name).map(name -> _)
      case (_, m) =>
        logger.error(s"Unexpected: '$m' cannot be an arrow")
        None
    }
  }

  private def getAbilityArrows[S: Arrows: Exports](
    abilityName: String,
    abilityType: AbilityType
  ): State[S, Map[String, FuncArrow]] = for {
    exports <- Exports[S].exports
    arrows <- Arrows[S].arrows
  } yield getAbilityArrows(abilityName, None, abilityType, exports, arrows)

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
    call: CallModel,
    oldExports: Map[String, ValueModel],
    arrows: Map[String, FuncArrow]
  ): State[S, (RawTag.Tree, List[ValueRaw])] =
    for {
      // Collect all arguments: what names are used inside the function, what values are received
      args <- State.pure(ArgsCall(fn.arrowType.domain, call.args))

      abArgs = args.abilityArgs

      abilityResolvingResult <- abArgs.toList.traverse { case (str, (vm, sct)) =>
        renameAndResolveAbilities(str, vm, sct, oldExports, arrows)
      }.map(_.combineAll)

      absRenames = abilityResolvingResult.namesToRename
      absVars = abilityResolvingResult.renamedExports
      absArrows = abilityResolvingResult.renamedArrows

      arrowArgs = args.arrowArgs(arrows)
      // Update states and rename tags
      renamedArrows <- updateArrowsAndRenameArrowArgs(arrowArgs ++ absArrows, fn, absRenames)

      argsToDataShouldRename <- updateExportsAndRenameDataArgs(args.dataArgs ++ absVars, absRenames)

      // rename variables that store arrows
      _ <- Exports[S].renameVariables(renamedArrows)

      /*
       * Don't rename arrows from abilities in a body, because we link arrows by VarModel
       * and they won't be called directly.
       * They could intersect with arrows defined inside the body
       *
       *  ability Simple:
       *   arrow() -> bool
       *
       *  func foo{Simple}() -> bool, bool:
       *   closure = () -> bool:
       *       <- true
       *   <- closure(), Simple.arrow()
       *
       *  func main() -> bool, bool:
       *   closure = () -> bool:
       *       <- false
       *   MySimple = Simple(arrow = closure)
       *   -- here we will rename arrow in Arrows[S] to 'closure-0'
       *   -- and link to arrow as 'Simple.arrow' -> VarModel('closure-0')
       *   -- and it will work well with closure with the same name 'closure' inside 'foo'
       *   foo{MySimple}()
       */
      allShouldRename = argsToDataShouldRename ++ (renamedArrows -- absArrows.keySet) ++ absRenames

      // Rename all renamed arguments in the body
      treeRenamed = fn.body.rename(allShouldRename)
      treeStreamsRenamed = renameStreams(
        treeRenamed,
        args.streamArgs.map { case (k, v) => argsToDataShouldRename.getOrElse(k, k) -> v }
      )

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
      arrowsFromAbilities <- call.abilityArgs
        .traverse(getAbilityArrows.tupled)
        .map(_.flatMap(_.toList).toMap)

      exports <- Exports[S].exports
      streams <- getOutsideStreamNames

      inlineResult <- Exports[S].scope(
        Arrows[S].scope(
          for {
            // Process renamings, prepare environment
            tr <- prelude[S](arrow, call, exports, passArrows ++ arrowsFromAbilities)
            (tree, results) = tr
            inlineResult <- ArrowInliner.inline(
              arrow.copy(body = tree, ret = results),
              call,
              streams
            )
          } yield inlineResult
        )
      )

      _ <- Arrows[S].resolved(inlineResult.arrowsToSave)
      _ <- Exports[S].resolved(
        call.exportTo
          .map(_.name)
          .zip(inlineResult.returnedValues)
          .toMap ++ inlineResult.exportsToSave
      )
    } yield inlineResult.tree -> inlineResult.returnedValues
}
