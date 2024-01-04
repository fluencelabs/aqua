package aqua.model.inline.state

import aqua.model.FuncArrow
import aqua.model.ValueModel.Ability
import aqua.model.{LiteralModel, ValueModel, VarModel}
import aqua.types.ServiceType
import aqua.types.{AbilityType, GeneralAbilityType, NamedType}

import cats.data.{NonEmptyList, State}

/**
 * Exports – trace values available in the scope
 * @tparam S - State
 */
trait Exports[S] extends Scoped[S] {
  self =>

  /**
   * [[exp]] is accessible as [[name]]
   * @param name - Name
   * @param exp - Export
   */
  def resolved(name: String, exp: Exports.Export): State[S, Unit]

  /**
   * Resolve the whole map of exports
   * @param exports - name -> export
   */
  def resolved(exports: Map[String, Exports.Export]): State[S, Unit]

  /**
   * Rename names in variables
   * @param renames - oldName -> newName
   */
  def renameExports(renames: Map[String, String]): State[S, Unit]

  /**
   * Get export by name
   * @param name - Name
   */
  def get(name: String): State[S, Option[Exports.Export]]

  /**
   * Change [[S]] to [[R]]
   */
  def transformS[R](f: R => S, g: (R, S) => R): Exports[R] = new Exports[R] {

    override def resolved(name: String, exp: Exports.Export): State[R, Unit] =
      self.resolved(name, exp).transformS(f, g)

    override def resolved(exports: Map[String, Exports.Export]): State[R, Unit] =
      self.resolved(exports).transformS(f, g)

    override def renameExports(renames: Map[String, String]): State[R, Unit] =
      self.renameExports(renames).transformS(f, g)

    override def get(name: String): State[R, Option[Exports.Export]] =
      self.get(name).transformS(f, g)
  }
}

object Exports {

  enum Export {
    case Value(value: ValueModel)
    case Arrow(arrow: FuncArrow)
    case Ability(abilityType: AbilityType, values: Map[String, Export])
    case Service(serviceType: ServiceType, arrows: Map[String, Arrow])
    case Context(values: Map[String, Export])
  }

  object Export {

    extension (c: Export.Context) {

      def resolved(name: String, exp: Export): Export.Context =
        c.copy(values = c.values + (name -> exp))

      def resolved(exports: Map[String, Export]): Export.Context =
        c.copy(values = c.values ++ exports)

      def renameExports(renames: Map[String, String]): Export.Context =
        c.copy(values = c.values.map { case (name, exp) =>
          renames.getOrElse(name, name) -> exp
        })
    }
  }

  def apply[S](using exports: Exports[S]): Exports[S] = exports

  object Simple extends Exports[Export.Context] {

    type ST[A] = State[Export.Context, A]

    override def resolved(name: String, exp: Export): ST[Unit] =
      State.modify(_.resolved(name, exp))

    override def resolved(exports: Map[String, Export]): ST[Unit] =
      State.modify(_.resolved(exports))

    override def renameExports(renames: Map[String, String]): ST[Unit] =
      State.modify(_.renameExports(renames))

    override def get(name: String): ST[Option[Export]] =
      State.inspect(_.values.get(name))
  }
}
