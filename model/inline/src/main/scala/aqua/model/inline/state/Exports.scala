package aqua.model.inline.state

import aqua.model.ValueModel.Ability
import aqua.model.{LiteralModel, ValueModel, VarModel}
import aqua.types.StreamType
import aqua.types.{AbilityType, GeneralAbilityType, NamedType}

import cats.data.{NonEmptyList, State}
import cats.syntax.apply.*
import cats.syntax.traverse.*

/**
 * Exports – trace values available in the scope
 * @tparam S
 *   State
 */
trait Exports[S] extends Scoped[S] {
  self =>

  /**
   * [[value]] is accessible as [[exportName]]
   * @param exportName
   *   Name
   * @param value
   *   Value
   */
  def resolved(exportName: String, value: ValueModel): State[S, Unit]

  /**
   * [[value]] is accessible as [[abilityExportName]].[[fieldName]]
   *
   * @param abilityExportName
   * Ability Name
   * @param fieldName
   * Field Name
   * @param value
   * Value
   */
  def resolveAbilityField(
    abilityExportName: String,
    fieldName: String,
    value: ValueModel
  ): State[S, Unit]

  /**
   * Rename ability prefix to new one
   */
  def copyWithAbilityPrefix(prefix: String, newPrefix: String): State[S, Unit]

  /**
   * Get name of last linked VarModel. If the last element is not VarModel, return None
   */
  def getLastVarName(name: String): State[S, Option[String]]

  /**
   * Resolve the whole map of exports
   * @param exports
   *   name -> value
   */
  def resolved(exports: Map[String, ValueModel]): State[S, Unit]

  /**
   * Get all export keys
   */
  def getKeys: State[S, Set[String]]

  /**
   * Get ability field from export
   * @param name variable ability name
   * @param field ability field
   */
  def getAbilityField(name: String, field: String): State[S, Option[ValueModel]]

  /**
   * Get all the values available in the scope
   */
  def exports: State[S, Map[String, ValueModel]]

  def addStream(name: String, streamType: StreamType): State[S, Unit]
  def deleteStreams(names: Set[String]): State[S, Unit]

  def streams: State[S, Map[String, StreamType]]

  final def gather(names: Seq[String]): State[S, Map[String, ValueModel]] =
    exports.map(Exports.gatherFrom(names, _))

  /**
   * Change [[S]] to [[R]]
   */
  def transformS[R](f: R => S, g: (R, S) => R): Exports[R] = new Exports[R] {

    override def resolved(exportName: String, value: ValueModel): State[R, Unit] =
      self.resolved(exportName, value).transformS(f, g)

    override def resolved(exports: Map[String, ValueModel]): State[R, Unit] =
      self.resolved(exports).transformS(f, g)


    override def streams: State[R, Map[String, StreamType]] =
      self.streams.transformS(f, g)

    override def addStream(name: String, streamType: StreamType): State[R, Unit] =
      self.addStream(name, streamType).transformS(f, g)

    override def deleteStreams(names: Set[String]): State[R, Unit] =
      self.deleteStreams(names).transformS(f, g)

    override def resolveAbilityField(
      abilityExportName: String,
      fieldName: String,
      value: ValueModel
    ): State[R, Unit] =
      self.resolveAbilityField(abilityExportName, fieldName, value).transformS(f, g)

    override def copyWithAbilityPrefix(prefix: String, newPrefix: String): State[R, Unit] =
      self.copyWithAbilityPrefix(prefix, newPrefix).transformS(f, g)

    override def getLastVarName(name: String): State[R, Option[String]] =
      self.getLastVarName(name).transformS(f, g)

    override def getKeys: State[R, Set[String]] =
      self.getKeys.transformS(f, g)

    override def getAbilityField(name: String, field: String): State[R, Option[ValueModel]] =
      self.getAbilityField(name, field).transformS(f, g)

    override def exports: State[R, Map[String, ValueModel]] =
      self.exports.transformS(f, g)

    override protected def purge: State[R, R] =
      self.purgeR(f, g)

    override protected def set(r: R): State[R, Unit] =
      self.setR(f, g)(r)
  }
}

object Exports {
  def apply[S](using exports: Exports[S]): Exports[S] = exports

  /**
   * Gather all the values that are related to the given names
   * (ability fields)
   *
   * @param names names of variables
   * @param state exports state
   */
  def gatherFrom(
    names: Seq[String],
    state: Map[String, ValueModel]
  ): Map[String, ValueModel] = {
    val related = for {
      variable <- names
      exp <- state.get(variable).toList
      at <- exp.`type` match {
        case at: GeneralAbilityType => at :: Nil
        case _ => Nil
      }
      field <- at.allFields.toNel.toList
      (fieldName, _) = field
    } yield AbilityType.fullName(variable, fieldName)

    state.view.filterKeys(names.toSet ++ related).toMap
  }

  // Get last linked VarModel
  def getLastValue(name: String, state: Map[String, ValueModel]): Option[ValueModel] = {
    state.get(name) match {
      case Some(vm @ VarModel(n, _, _)) =>
        if (name == n) Option(vm)
        else getLastValue(n, state).orElse(Option(vm))
      case lm @ Some(LiteralModel(_, _)) =>
        lm
      case _ =>
        None
    }
  }

  case class ExportsState(values: Map[String, ValueModel] = Map.empty, streams: Map[String, StreamType] = Map.empty)

  object Simple extends Exports[ExportsState] {

    // Make links from one set of abilities to another (for ability assignment)
    private def getAbilityPairs(
      oldName: String,
      newName: String,
      at: NamedType,
      state: Map[String, ValueModel]
    ): NonEmptyList[(String, ValueModel)] = {
      at.fields.toNel.flatMap {
        case (n, at @ AbilityType(_, _)) =>
          val newFullName = AbilityType.fullName(newName, n)
          val oldFullName = AbilityType.fullName(oldName, n)
          getAbilityPairs(oldFullName, newFullName, at, state)
        case (n, t) =>
          val newFullName = AbilityType.fullName(newName, n)
          val oldFullName = AbilityType.fullName(oldName, n)
          // put link on last variable in chain
          val lastVar = Exports.getLastValue(oldFullName, state)
          NonEmptyList.of((newFullName, lastVar.getOrElse(VarModel(oldFullName, t))))
      }
    }

    override def resolved(
      exportName: String,
      value: ValueModel
    ): State[ExportsState, Unit] = State.modify { state =>
      val newValues = value match {
        case Ability(vm, at) if vm.properties.isEmpty =>
          val pairs = getAbilityPairs(vm.name, exportName, at, state.values)
          state.values ++ pairs.toList.toMap + (exportName -> value)
        case _ => state.values + (exportName -> value)
      }
      state.copy(values = newValues)
    }

    override def getLastVarName(name: String): State[ExportsState, Option[String]] =
      State.get.map(st => getLastValue(name, st.values).collect { case VarModel(name, _, _) => name })

    override def resolved(exports: Map[String, ValueModel]): State[ExportsState, Unit] =
      State.modify(st => st.copy(values = st.values ++ exports))

    override def streams: State[ExportsState, Map[String, StreamType]] =
      State.get.map(_.streams)

    override def addStream(name: String, streamType: StreamType): State[ExportsState, Unit] =
      State.modify(st => st.copy(streams = st.streams + (name -> streamType)))

    override def deleteStreams(names: Set[String]): State[ExportsState, Unit] =
      State.modify(st => st.copy(streams = st.streams -- names))

    override def resolveAbilityField(
      abilityExportName: String,
      fieldName: String,
      value: ValueModel
    ): State[ExportsState, Unit] =
      State.modify(st => st.copy(values = st.values + (AbilityType.fullName(abilityExportName, fieldName) -> value)))

    override def copyWithAbilityPrefix(
      prefix: String,
      newPrefix: String
    ): State[ExportsState, Unit] =
      State.modify { state =>
        val newValues = state.values.flatMap {
          case (k, v) if k.startsWith(prefix) =>
            List(k.replaceFirst(prefix, newPrefix) -> v, k -> v)
          case (k, v) => List(k -> v)
        }
        state.copy(values = newValues)
      }

    override def getKeys: State[ExportsState, Set[String]] = State.get.map(_.values.keySet)

    override def getAbilityField(
      name: String,
      field: String
    ): State[ExportsState, Option[ValueModel]] =
      State.get.map(_.values.get(AbilityType.fullName(name, field)))

    override val exports: State[ExportsState, Map[String, ValueModel]] =
      State.get.map(_.values)

    override val purge: State[ExportsState, ExportsState] =
      for {
        st <- State.get
        _ <- State.modify[ExportsState](st => ExportsState(streams = st.streams))
      } yield st


    override def set(s: ExportsState): State[ExportsState, Unit] = {
      for {
        st <- State.get
        _ <- State.set(s.copy(streams = st.streams ++ s.streams))
      } yield {}
    }
  }
}
