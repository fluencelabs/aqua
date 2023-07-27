package aqua.model.inline.state

import aqua.model.{ValueModel, VarModel}
import aqua.types.AbilityType
import cats.data.State

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
  def renameAbilityPrefix(prefix: String, newPrefix: String): State[S, Unit]

  /**
   * Rename names in variables
   */
  def renameVariables(renames: Map[String, String]): State[S, Unit]

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
  val exports: State[S, Map[String, ValueModel]]

  /**
   * Change [[S]] to [[R]]
   */
  def transformS[R](f: R => S, g: (R, S) => R): Exports[R] = new Exports[R] {

    override def resolved(exportName: String, value: ValueModel): State[R, Unit] =
      self.resolved(exportName, value).transformS(f, g)

    override def resolved(exports: Map[String, ValueModel]): State[R, Unit] =
      self.resolved(exports).transformS(f, g)

    override def resolveAbilityField(
      abilityExportName: String,
      fieldName: String,
      value: ValueModel
    ): State[R, Unit] =
      self.resolveAbilityField(abilityExportName, fieldName, value).transformS(f, g)

    override def renameAbilityPrefix(prefix: String, newPrefix: String): State[R, Unit] =
      self.renameAbilityPrefix(prefix, newPrefix).transformS(f, g)

    override def renameVariables(renames: Map[String, String]): State[R, Unit] =
      self.renameVariables(renames).transformS(f, g)

    override def getKeys: State[R, Set[String]] =
      self.getKeys.transformS(f, g)

    override def getAbilityField(name: String, field: String): State[R, Option[ValueModel]] =
      self.getAbilityField(name, field).transformS(f, g)

    override val exports: State[R, Map[String, ValueModel]] =
      self.exports.transformS(f, g)

    override val purge: State[R, R] =
      self.purgeR(f, g)

    override protected def fill(s: R): State[R, Unit] =
      self.fillR(s, f, g)
  }
}

object Exports {
  def apply[S](implicit exports: Exports[S]): Exports[S] = exports

  object Simple extends Exports[Map[String, ValueModel]] {

    override def resolved(
      exportName: String,
      value: ValueModel
    ): State[Map[String, ValueModel], Unit] = State.modify(_ + (exportName -> value))

    override def resolved(exports: Map[String, ValueModel]): State[Map[String, ValueModel], Unit] =
      State.modify(_ ++ exports)

    override def resolveAbilityField(
      abilityExportName: String,
      fieldName: String,
      value: ValueModel
    ): State[Map[String, ValueModel], Unit] =
      State.modify(_ + (AbilityType.fullName(abilityExportName, fieldName) -> value))

    override def renameAbilityPrefix(
      prefix: String,
      newPrefix: String
    ): State[Map[String, ValueModel], Unit] =
      State.modify { state =>
        state.map {
          case (k, v) if k.startsWith(prefix) =>
            k.replaceFirst(prefix, newPrefix) -> v
          case (k, v) => k -> v
        }
      }

    override def renameVariables(
      renames: Map[String, String]
    ): State[Map[String, ValueModel], Unit] =
      State.modify {
        _.map {
          case (k, vm @ VarModel(name, _, _)) if renames.contains(name) =>
            k -> vm.copy(name = renames.getOrElse(name, name))
          case (k, v) => k -> v
        }
      }

    override def getKeys: State[Map[String, ValueModel], Set[String]] = State.get.map(_.keySet)

    override def getAbilityField(
      name: String,
      field: String
    ): State[Map[String, ValueModel], Option[ValueModel]] =
      State.get.map(_.get(AbilityType.fullName(name, field)))

    override val exports: State[Map[String, ValueModel], Map[String, ValueModel]] =
      State.get

    override val purge: State[Map[String, ValueModel], Map[String, ValueModel]] =
      for {
        s <- State.get
        _ <- State.set(Map.empty)
      } yield s

    override protected def fill(s: Map[String, ValueModel]): State[Map[String, ValueModel], Unit] =
      State.set(s)
  }
}
