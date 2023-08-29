package aqua.model.inline

import aqua.model
import aqua.model.*
import aqua.model.inline.state.{Arrows, Exports, Mangler}
import aqua.raw.ops.RawTag
import aqua.raw.value.{ValueRaw, VarRaw}
import aqua.types.{AbilityType, ArrowType, BoxType, StreamType, Type}

import cats.data.StateT
import cats.data.{Chain, IndexedStateT, State}
import cats.syntax.applicative.*
import cats.syntax.bifunctor.*
import cats.syntax.foldable.*
import cats.syntax.traverse.*
import cats.syntax.option.*
import cats.syntax.show.*
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
  ): State[S, InlineResult] = for {
    callableFuncBodyNoTopology <- TagInliner.handleTree(fn.body, fn.funcName)
    callableFuncBody =
      fn.capturedTopology
        .fold(SeqModel)(ApplyTopologyModel.apply)
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

  /**
   * Get ability fields (vars or arrows) from exports
   *
   * @param abilityName ability current name in state
   * @param abilityNewName ability new name (for renaming)
   * @param abilityType ability type
   * @param exports exports state to resolve fields
   * @param fields fields selector
   * @return resolved ability fields (renamed if necessary)
   */
  private def getAbilityFields[T <: Type](
    abilityName: String,
    abilityNewName: Option[String],
    abilityType: AbilityType,
    exports: Map[String, ValueModel]
  )(fields: AbilityType => Map[String, T]): Map[String, ValueModel] =
    fields(abilityType).flatMap { case (fName, _) =>
      val fullName = AbilityType.fullName(abilityName, fName)
      val newFullName = AbilityType.fullName(abilityNewName.getOrElse(abilityName), fName)

      Exports
        .getLastValue(fullName, exports)
        .map(newFullName -> _)
    }

  /**
   * Get ability vars and arrows as vars from exports
   *
   * @param abilityName ability current name in state
   * @param abilityNewName ability new name (for renaming)
   * @param abilityType ability type
   * @param exports exports state to resolve fields
   * @return resolved ability vars and arrows as vars (renamed if necessary)
   */
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

  /**
   * Get ability arrows from arrows
   *
   * @param abilityName ability current name in state
   * @param abilityNewName ability new name (for renaming)
   * @param abilityType ability type
   * @param exports exports state to resolve fields
   * @param arrows arrows state to resolve arrows
   * @return resolved ability arrows (renamed if necessary)
   */
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
  ): State[S, FuncArrow] = for {
    // Collect all arguments: what names are used inside the function, what values are received
    args <- ArgsCall(fn.arrowType.domain, call.args).pure[State[S, *]]

    // forbidden <- Mangler[S].getForbiddenNames

    argNames = args.argNames
    dataArgs = args.dataArgs
    streamArgs = args.streamArgs
    arrowArgs = args.arrowArgs
    abArgs = args.abilityArgs

    dataRenames <- Mangler[S].findAndForbidNames(dataArgs.keySet)
    dataRenamed = dataArgs.map { case (name, vm) =>
      dataRenames.getOrElse(name, name) -> vm
    }

    streamRenames = streamArgs.toList.map { case (name, vm) =>
      name -> vm.name
    }.toMap

    arrowRenames = arrowArgs.toList.map { case (name, vm) =>
      name -> vm.name
    }.toMap

    abRenames = abArgs.toList.foldMap { case (name, (vm, at)) =>
      at.arrows.keys
        .map(arrowPath =>
          val fullName = AbilityType.fullName(name, arrowPath)
          val newFullName = AbilityType.fullName(vm.name, arrowPath)
          fullName -> newFullName
        )
        .toMap
        .updated(name, vm.name)
    }

    capturedValuesRenames <- Mangler[S].findAndForbidNames(fn.capturedValues.keySet)
    capturedValuesRenamed = fn.capturedValues.map { case (name, vm) =>
      capturedValuesRenames.getOrElse(name, name) -> vm
    }

    capturedArrowsRenames <- Mangler[S].findAndForbidNames(fn.capturedArrows.keySet)
    capturedArrowsRenamed = fn.capturedArrows.map { case (name, vm) =>
      capturedArrowsRenames.getOrElse(name, name) -> vm
    }

    defineNames <- StateT.liftF(fn.body.definesVarNames.map(_ -- argNames))
    defineRenames <- Mangler[S].findAndForbidNames(defineNames)

    renaming = (
      dataRenames ++
        streamRenames ++
        arrowRenames ++
        abRenames ++
        capturedValuesRenames ++
        capturedArrowsRenames ++
        defineRenames
    )

    arrowsResolved = arrows ++ capturedArrowsRenamed
    exportsResolved = oldExports ++ dataRenamed ++ capturedValuesRenamed

    tree = fn.body.rename(renaming)
    ret = fn.ret.map(_.renameVars(renaming))

    // _ = println(s"\n\nPrelude: ${fn.funcName}")
    // _ = println(s"Forbidden: $forbidden")
    // _ = println(s"Captured arrows: ${fn.capturedArrows.keySet}")
    // _ = println(s"Domain: ${fn.arrowType.domain}")
    // _ = println(s"Body: \n${fn.body.show}`")
    // _ = println(s"Arrows: $arrowsResolved")
    // _ = println(s"Exports: $exportsResolved")
    // _ = println(s"Data args: ${dataArgs}")
    // _ = println(s"Ab args: ${abArgs.keySet}")
    // _ = println(s"Arrow args: ${arrowArgs.keySet}")
    // _ = println(s"Stream args: ${streamArgs.keySet}")
    // _ = println(s"Data renames: $dataRenames")
    // _ = println(s"Arrow renames: $arrowRenames")
    // _ = println(s"Stream renames: $streamRenames")
    // _ = println(s"Ab renames: $abRenames")
    // _ = println(s"Define names: $defineNames")
    // _ = println(s"Define renames: $defineRenames")
    // _ = println(s"Renaming: $renaming")
    // _ = println(s"Tree: \n${tree.show}")
    // _ = println(s"Ret: \n${ret}")

    _ <- Arrows[S].resolved(arrowsResolved)
    _ <- Exports[S].resolved(exportsResolved)
  } yield fn.copy(body = tree, ret = ret)

  private[inline] def callArrowRet[S: Exports: Arrows: Mangler](
    arrow: FuncArrow,
    call: CallModel
  ): State[S, (OpModel.Tree, List[ValueModel])] = for {
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
          fn <- prelude[S](arrow, call, exports, passArrows ++ arrowsFromAbilities)
          inlineResult <- ArrowInliner.inline(fn, call, streams)
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
