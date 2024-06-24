/*
 * Copyright (C) 2024  Fluence DAO
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package aqua.model.inline

import aqua.errors.Errors.internalError
import aqua.model
import aqua.model.*
import aqua.model.inline.state.*
import aqua.raw.ops.RawTag
import aqua.raw.value.{ValueRaw, VarRaw}
import aqua.types.*

import cats.data.{Chain, IndexedStateT, State, StateT}
import cats.free.Cofree
import cats.kernel.Semigroup
import cats.syntax.applicative.*
import cats.syntax.bifunctor.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.option.*
import cats.syntax.semigroup.*
import cats.syntax.traverse.*
import cats.{Eval, Monoid}
import scala.annotation.tailrec
import scribe.Logging

/**
 * Does [[RawTag.Tree]] -> [[OpModel.Tree]] transformation:
 * - Converts [[ValueRaw]] to [[ValueModel]] via [[TagInliner]]
 * - For [[aqua.raw.ops.CallArrowTag]]
 */
object ArrowInliner extends Logging {

  def callArrow[S: Exports: Arrows: Mangler: Config](
    arrow: FuncArrow,
    call: CallModel
  ): State[S, OpModel.Tree] =
    callArrowRet(arrow, call).map { case (tree, _) => tree }

  // Get streams that was declared outside of a function
  private def getOutsideStreamNames[S: Exports]: State[S, Set[String]] =
    Exports[S].exports
      .map(exports =>
        exports.collect { case (n, VarModel(_, _: StreamType, _)) =>
          n
        }.toSet
      )

  // push results to streams if they are exported to streams
  private def pushStreamResults[S: Mangler: Exports: Arrows: Config](
    outsideStreamNames: Set[String],
    exportTo: List[CallModel.Export],
    results: List[ValueRaw]
  ): State[S, (List[OpModel.Tree], List[ValueModel])] =
    // Fix return values with exports collected in the body
    RawValueInliner
      .valueListToModel(results)
      .map(resolvedResults =>
        // Fix the return values
        (exportTo zip resolvedResults).map {
          case (
                CallModel.Export(n, StreamType(_)),
                (res @ VarModel(_, StreamType(_), _), resDesugar)
              ) if !outsideStreamNames.contains(n) =>
            resDesugar.toList -> res
          case (
                cexp @ CallModel.Export(_, StreamType(_)),
                (res, resDesugar)
              ) =>
            // pass nested function results to a stream
            (resDesugar.toList :+ PushToStreamModel(res, cexp).leaf) -> cexp.asVar
          case (_, (res, resDesugar)) =>
            resDesugar.toList -> res
        }.unzip.leftMap(_.flatten)
      )

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

  /**
   * Find abilities recursively, because ability can hold arrow with another ability in it.
   * @param abilitiesToGather gather all fields for these abilities
   * @param varsFromAbs already gathered variables
   * @param arrowsFromAbs already gathered arrows
   * @param processedAbs already processed abilities
   * @return all needed variables and arrows
   */
  @tailrec
  private def arrowsAndVarsFromAbilities(
    abilitiesToGather: Map[String, GeneralAbilityType],
    exports: Map[String, ValueModel],
    arrows: Map[String, FuncArrow],
    varsFromAbs: Map[String, ValueModel] = Map.empty,
    arrowsFromAbs: Map[String, FuncArrow] = Map.empty,
    processedAbs: Set[String] = Set.empty
  ): (Map[String, ValueModel], Map[String, FuncArrow]) = {
    val varsFromAbilities = abilitiesToGather.flatMap { case (name, at) =>
      getAbilityVars(name, None, at, exports)
    }
    val arrowsFromAbilities = abilitiesToGather.flatMap { case (name, at) =>
      getAbilityArrows(name, None, at, exports, arrows)
    }

    val allProcessed = abilitiesToGather.keySet ++ processedAbs

    // find all names that is used in arrows
    val namesUsage = arrowsFromAbilities.values.flatMap(_.body.usesVarNames.value).toSet

    // check if there is abilities that we didn't gather
    val abilitiesUsage = namesUsage.toList
      .flatMap(exports.get)
      .collect {
        case ValueModel.Ability(vm, at) if !allProcessed.contains(vm.name) =>
          vm.name -> at
      }
      .toMap

    val allVars = varsFromAbilities ++ varsFromAbs
    val allArrows = arrowsFromAbilities ++ arrowsFromAbs

    if (abilitiesUsage.isEmpty) {
      (allVars, allArrows)
    } else {
      arrowsAndVarsFromAbilities(
        abilitiesUsage,
        exports,
        arrows,
        allVars,
        allArrows,
        allProcessed
      )
    }

  }

  // Apply a callable function, get its fully resolved body & optional value, if any
  private def inline[S: Mangler: Arrows: Exports: Config](
    fn: FuncArrow,
    call: CallModel,
    outsideDeclaredStreams: Set[String]
  ): State[S, InlineResult] = for {
    callableFuncBodyNoTopology <- TagInliner.handleTree(fn.body)

    callableFuncBody =
      fn.capturedTopology
        .fold(SeqModel)(ApplyTopologyModel.apply)
        .wrap(callableFuncBodyNoTopology)

    opsAndRets <- pushStreamResults(
      outsideStreamNames = outsideDeclaredStreams,
      exportTo = call.exportTo,
      results = fn.ret
    )
    (ops, rets) = opsAndRets

    exports <- Exports[S].exports
    arrows <- Arrows[S].arrows
    // gather all arrows and variables from abilities
    abilitiesToGather = rets.collect { case ValueModel.Ability(vm, at) =>
      vm.name -> at
    }
    arrsVars = arrowsAndVarsFromAbilities(
      abilitiesToGather.toMap,
      exports,
      arrows
    )
    (varsFromAbilities, arrowsFromAbilities) = arrsVars

    // find and get resolved arrows if we return them from the function
    returnedArrows = rets.collect { case VarModel(name, _: ArrowType, _) => name }.toSet
    arrowsFromClosures <- Arrows[S].pickArrows(returnedArrows)
    arrowsToSave = arrowsFromAbilities ++ arrowsFromClosures

    body = SeqModel.wrap(callableFuncBody :: ops)
  } yield InlineResult(
    body,
    rets,
    varsFromAbilities,
    arrowsToSave
  )

  /**
   * Get ability fields (vars or arrows) from exports
   *
   * @param name ability current name in state
   * @param newName ability new name (for renaming)
   * @param type ability type
   * @param exports exports state to resolve fields
   * @param fields fields selector
   * @return resolved ability fields (renamed if necessary)
   */
  private def getAbilityFields[T <: Type](
    name: String,
    newName: Option[String],
    `type`: GeneralAbilityType,
    exports: Map[String, ValueModel]
  )(fields: GeneralAbilityType => Map[String, T]): Map[String, ValueModel] =
    fields(`type`).flatMap { case (fName, _) =>
      val fullName = AbilityType.fullName(name, fName)
      val newFullName = AbilityType.fullName(newName.getOrElse(name), fName)

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
    abilityType: GeneralAbilityType,
    exports: Map[String, ValueModel]
  ): Map[String, ValueModel] = {
    val get = getAbilityFields(
      abilityName,
      abilityNewName,
      abilityType,
      exports
    )
    val abilityExport =
      exports.get(abilityName).map(vm => abilityNewName.getOrElse(abilityName) -> vm).toMap

    abilityExport ++ get(_.variables) ++ get(_.arrows).flatMap {
      case arrow @ (_, ValueModel.Arrow(_, _)) =>
        arrow.some
      case (_, m) =>
        internalError(s"($m) cannot be an arrow for '$abilityName' ability")
    }
  }

  /**
   * Get ability arrows from arrows
   *
   * @param name ability current name in state
   * @param newName ability new name (for renaming)
   * @param type ability type
   * @param exports exports state to resolve fields
   * @param arrows arrows state to resolve arrows
   * @return resolved ability arrows (renamed if necessary)
   */
  private def getAbilityArrows(
    name: String,
    newName: Option[String],
    `type`: GeneralAbilityType,
    exports: Map[String, ValueModel],
    arrows: Map[String, FuncArrow]
  ): Map[String, FuncArrow] = {
    val get = getAbilityFields(
      name,
      newName,
      `type`,
      exports
    )

    get(_.arrows).flatMap {
      case (_, ValueModel.Arrow(vm, _)) =>
        arrows.get(vm.name).map(vm.name -> _)
      case (_, m) =>
        internalError(s"($m) cannot be an arrow for '$name' ability")
    }
  }

  private def getAbilityArrows[S: Arrows: Exports](
    name: String,
    `type`: GeneralAbilityType
  ): State[S, Map[String, FuncArrow]] = for {
    exports <- Exports[S].exports
    arrows <- Arrows[S].arrows
  } yield getAbilityArrows(name, None, `type`, exports, arrows)

  final case class Renamed[T](
    renames: Map[String, String],
    renamed: Map[String, T]
  )

  given [T]: Monoid[Renamed[T]] with {
    override def empty: Renamed[T] = Renamed(Map.empty, Map.empty)

    override def combine(x: Renamed[T], y: Renamed[T]): Renamed[T] =
      Renamed(
        x.renames ++ y.renames,
        x.renamed ++ y.renamed
      )
  }

  // TODO: Make this extension private somehow?
  extension [T](vals: Map[String, T]) {

    def renamed(renames: Map[String, String]): Map[String, T] =
      vals.map { case (name, value) =>
        renames.getOrElse(name, name) -> value
      }
  }

  /**
   * Rename values and forbid new names
   *
   * @param values Mapping name -> value
   * @return Renamed values and renames
   */
  private def findNewNames[S: Mangler, T](
    values: Map[String, T]
  ): State[S, Renamed[T]] =
    Mangler[S].findAndForbidNames(values.keySet).map { renames =>
      Renamed(
        renames,
        values.renamed(renames)
      )
    }

  /**
   * Correctly rename captured values and arrows of a function
   *
   * @param fn Function
   * @return Renamed values and arrows
   */
  def renameCaptured[S: Mangler](
    fn: FuncArrow
  ): State[S, (Renamed[ValueModel], Renamed[FuncArrow])] = {
    // Gather abilities related values
    val abilitiesValues = fn.capturedValues.collect {
      // Gather only top level abilities
      case (name, ValueModel.Ability(vm, at)) if vm.properties.isEmpty =>
        name -> (
          at,
          /**
           * Gather all values related to `name`
           * NOTE: It is important that `capturedValues` are
           * populated by all values related to ability `name`
           * on creation of `FuncArrow`.
           */
          Exports.gatherFrom(
            name :: Nil,
            fn.capturedValues
          )
        )
    }
    // Gather all abilities related names
    val abilitiesValuesKeys = abilitiesValues.flatMap { case (_, (_, values)) =>
      values.keySet
    }

    // Gather abilities related arrows
    val abilitiesArrows = abilitiesValues.toList.foldMap { case (_, (_, values)) =>
      Arrows.arrowsByValues(fn.capturedArrows, values).toList
    }.toMap

    // Gather all other values and arrows that are not related to abilities
    val otherValues = fn.capturedValues -- abilitiesValuesKeys
    val otherArrows = fn.capturedArrows -- abilitiesArrows.keySet

    for {
      // Calculate renaming based on abilities
      valuesRenamed <- abilitiesValues.toList.traverse { case (name, (at, values)) =>
        Mangler[S]
          .findAndForbidName(name)
          .map(rename =>
            // Get renaming map for this ability
            AbilityType
              .renames(at)(name, rename)
              // Add ability rename too
              .updated(name, rename)
          )
          .map(renames =>
            // This code is HACKERY!!!
            val valuesRenamed = values.renamed(renames).map {
              /**
               * `VarModel` is sometimes used to point to an arrow.
               * So if it is renamed, we should rename the `VarModel` too.
               * Otherwise renamed value will be resolved
               * to previous name when trying to resolve the arrow.
               * But this should be done only if the name in model
               * is the same as the name of the export,
               * because export could point to another arrow.
               */
              case (name, ValueModel.Arrow(vm, _)) if renames.contains(vm.name) =>
                name -> vm.copy(name = name)
              /**
               * `VarModel` is used to point to an ability.
               * So if it is renamed, we should rename the `VarModel` too.
               * Otherwise renamed value will be resolved
               * to previous name when trying to resolve the ability.
               */
              case (name, ValueModel.Ability(vm, _)) =>
                name -> vm.copy(name = name)
              case v => v
            }
            Renamed(renames, valuesRenamed)
          )
      }.map(_.combineAll)

      // Rename arrows according to values
      arrowsRenamed = Renamed(
        valuesRenamed.renames.view.filterKeys(abilitiesArrows.keySet).toMap,
        abilitiesArrows.renamed(valuesRenamed.renames)
      )

      // Rename values and arrows unrelated to abilities
      otherValuesRenamed <- findNewNames(otherValues)
      otherArrowsValues = Arrows.arrowsByValues(
        otherArrows,
        otherValues
      )
      otherArrowsValuesRenamed = Renamed(
        otherValuesRenamed.renames.view.filterKeys(otherArrowsValues.keySet).toMap,
        otherArrowsValues.renamed(otherValuesRenamed.renames)
      )

      otherArrowsRenamed <- findNewNames(otherArrows -- otherArrowsValues.keySet)

      values = valuesRenamed |+| otherValuesRenamed
      arrows = arrowsRenamed |+| otherArrowsValuesRenamed |+| otherArrowsRenamed
    } yield values -> arrows
  }

  // change all collections that passed as stream arguments to canonicalized streams
  private def collectionsToCanons(
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
          case v @ VarRaw(name, baseType: CollectionType) if streamsToRename.contains(name) =>
            v.copy(baseType = CanonStreamType(baseType.element))
          case v => v
        }))
        .renameExports(streamsToRename)
  }

  // Change the type of collection arguments if they pass as streams
  private def canonStreamVariables[S: Mangler](
    args: ArgsCall
  ): State[S, (Map[String, String], List[OpModel.Tree])] =
    args.streamToImmutableArgsWithTypes.toList.traverse { case (argName, (vm, StreamType(t))) =>
      Mangler[S].findAndForbidName(vm.name + "_canon").map { canonName =>
        (
          (argName, canonName),
          CanonicalizeModel(vm, CallModel.Export(canonName, CanonStreamType(t))).leaf
        )
      }
    }.map(_.unzip.leftMap(_.toMap))

  /**
   * Prepare the function and the context for inlining
   *
   * @param fn Function that will be called
   * @param call Call object
   * @param exports Exports state before calling/inlining
   * @param arrows Arrows that are available for callee
   * @return Prepared function
   */
  private def prelude[S: Mangler: Arrows: Exports](
    fn: FuncArrow,
    call: CallModel,
    exports: Map[String, ValueModel],
    arrows: Map[String, FuncArrow]
  ): State[S, (FuncArrow, OpModel.Tree)] = for {
    args <- ArgsCall(fn.arrowType.domain, call.args).pure[State[S, *]]

    /**
     * Substitute all arguments inside function body.
     * Data arguments could be passed as variables or values (expressions),
     * so we need to resolve them in `Exports`.
     * Streams, arrows, abilities are passed as variables only,
     * so we just rename them in the function body to match
     * the names in the current context.
     */
    data <- findNewNames(args.dataArgs)
    streamRenames = args.streamArgsRenames
    arrowRenames = args.arrowArgsRenames
    abRenames = args.abilityArgsRenames

    captured <- renameCaptured(fn)
    (capturedValues, capturedArrows) = captured

    /**
     * Function defines variables inside its body.
     * We rename and forbid all those names so that when we inline
     * **another function inside this one** we would know what names
     * are prohibited because they are used inside **this function**.
     */
    defineNames <- StateT.liftF(
      fn.body.definesVarNames
    )
    defineRenames <- Mangler[S].findAndForbidNames(defineNames)
    canonStreamsWithNames <- canonStreamVariables(args)
    (renamedCanonStreams, canons) = canonStreamsWithNames

    renaming =
      data.renames ++
        arrowRenames ++
        abRenames ++
        capturedValues.renames ++
        capturedArrows.renames ++
        defineRenames ++
        renamedCanonStreams ++
        streamRenames

    arrowsResolved = arrows ++ capturedArrows.renamed
    exportsResolved = exports ++ data.renamed ++ capturedValues.renamed

    tree = fn.body.rename(renaming)

    streamToCanonArgs = args.streamToImmutableArgs.renamed(renamedCanonStreams)
    treeWithCanons = collectionsToCanons(tree, streamToCanonArgs)

    ret = fn.ret.map(_.renameVars(renaming))

    _ <- Arrows[S].resolved(arrowsResolved)
    _ <- Exports[S].resolved(exportsResolved)
  } yield (fn.copy(body = treeWithCanons, ret = ret), SeqModel.wrap(canons))

  private[inline] def callArrowRet[S: Exports: Arrows: Mangler: Config](
    arrow: FuncArrow,
    call: CallModel
  ): State[S, (OpModel.Tree, List[ValueModel])] = for {
    passArrows <- Arrows[S].pickArrows(call.arrowArgNames)
    arrowsFromAbilities <- call.abilityArgs
      .traverse(getAbilityArrows.tupled)
      .map(_.flatMap(_.toList).toMap)

    exports <- Exports[S].exports
    streams <- getOutsideStreamNames
    arrows = passArrows ++ arrowsFromAbilities

    inlineResult <- Exports[S].scope(
      Arrows[S].scope(
        for {
          // Process renamings, prepare environment
          fnCanon <- ArrowInliner.prelude(arrow, call, exports, arrows)
          (fn, canons) = fnCanon
          inlineResult <- ArrowInliner.inline(fn, call, streams)
        } yield inlineResult.copy(tree = SeqModel.wrap(canons, inlineResult.tree))
      )
    )

    exportTo = call.exportTo.map(_.name)
    _ <- Arrows[S].resolved(inlineResult.arrowsToSave)
    _ <- Exports[S].resolved(
      exportTo
        .zip(inlineResult.returnedValues)
        .toMap ++ inlineResult.exportsToSave
    )
    _ <- Mangler[S].forbid(exportTo.toSet)
  } yield inlineResult.tree -> inlineResult.returnedValues
}
