package aqua.semantics.rules.definitions

import aqua.parser.lexer.{Name, NamedTypeToken, Token}
import aqua.semantics.rules.StackInterpreter
import aqua.semantics.rules.report.ReportErrors
import aqua.semantics.rules.abilities.AbilitiesState
import aqua.semantics.rules.locations.{LocationsAlgebra, LocationsState}
import aqua.semantics.rules.types.TypesState
import aqua.types.{ArrowType, Type}

import cats.data.{NonEmptyList, NonEmptyMap, State}
import monocle.Lens
import monocle.macros.GenLens
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.option.*

import scala.collection.immutable.SortedMap

class DefinitionsInterpreter[S[_], X](implicit
  lens: Lens[X, DefinitionsState[S]],
  error: ReportErrors[S, X],
  locations: LocationsAlgebra[S, State[X, *]]
) extends DefinitionsAlgebra[S, State[X, *]] {
  type SX[A] = State[X, A]

  private def getState = State.get.map(lens.get)

  private def modify(f: DefinitionsState[S] => DefinitionsState[S]): SX[Unit] =
    State.modify(lens.modify(f))

  def report(t: Token[S], hint: String): SX[Unit] =
    State.modify(error(_, t, hint :: Nil))

  def define(name: Name[S], `type`: Type, defName: String): SX[Boolean] =
    getState.map(_.definitions.get(name.value)).flatMap {
      case None =>
        modify(st =>
          st.copy(definitions =
            st.definitions.updated(
              name.value,
              DefinitionsState.Def(name, `type`)
            )
          )
        )
          .as(true)
      case Some(_) =>
        report(name, s"Cannot define $defName `${name.value}`, it was already defined above")
          .as(false)
    }

  override def defineDef(name: Name[S], `type`: Type): SX[Boolean] =
    define(name, `type`, "field")

  override def defineArrow(arrow: Name[S], `type`: ArrowType): SX[Boolean] =
    define(arrow, `type`, "arrow")

  override def purgeDefs(
    token: NamedTypeToken[S]
  ): SX[Map[String, DefinitionsState.Def[S]]] =
    getState.map(_.definitions).flatMap { defs =>
      val names = defs.view.mapValues(_.name)

      for {
        _ <- locations
          .addTokenWithFields(token.value, token, names.toList)
          .whenA(defs.nonEmpty)
        _ <- modify(_.copy(definitions = Map.empty))
      } yield defs
    }

  def purgeArrows(token: Token[S]): SX[Option[NonEmptyList[(Name[S], ArrowType)]]] =
    getState.map(_.definitions).flatMap { defs =>
      val arrows = defs.values.collect { case DefinitionsState.Def(name, t: ArrowType) =>
        name -> t
      }
      NonEmptyList.fromList(arrows.toList) match {
        case Some(arrs) =>
          modify { st =>
            st.copy(definitions = Map.empty)
          }.as(arrs.some)
        case None =>
          report(token, "Cannot purge arrows, no arrows provided")
            .as(none)
      }
    }
}
