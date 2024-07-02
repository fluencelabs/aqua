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

package aqua.semantics.rules.names

import aqua.errors.Errors.internalError
import aqua.parser.lexer.{Name, Token}
import aqua.semantics.Levenshtein
import aqua.semantics.rules.StackInterpreter
import aqua.semantics.rules.locations.{DefinitionInfo, LocationsAlgebra}
import aqua.semantics.rules.report.ReportAlgebra
import aqua.types.{ArrowType, StreamType, Type}

import cats.data.{OptionT, State}
import cats.syntax.all.*
import monocle.Lens
import monocle.macros.GenLens

class NamesInterpreter[S[_], X](using
  lens: Lens[X, NamesState[S]],
  report: ReportAlgebra[S, State[X, *]],
  locations: LocationsAlgebra[S, State[X, *]]
) extends NamesAlgebra[S, State[X, *]] {

  val stackInt = new StackInterpreter[S, X, NamesState[S], NamesState.Frame[S]](
    GenLens[NamesState[S]](_.stack)
  )

  import stackInt.*

  type SX[A] = State[X, A]

  private def readName(name: String): SX[Option[Type]] =
    getState.map { st =>
      st.constants.get(name) orElse st.stack.collectFirst {
        case frame if frame.names.contains(name) => frame.names(name)
        case frame if frame.arrows.contains(name) => frame.arrows(name)
      } orElse st.rootArrows.get(name)
    }

  override def read(name: Name[S], mustBeDefined: Boolean = true): SX[Option[Type]] =
    OptionT(constantDefined(name))
      .orElseF(readName(name.value))
      .value
      .flatTap {
        case None if mustBeDefined =>
          getState.flatMap(st =>
            report.error(
              name,
              Levenshtein
                .genMessage(
                  s"Name '${name.value}' isn't found in scope",
                  name.value,
                  st.allNames.toList
                )
            )
          )
        case Some(_) =>
          locations.pointLocation(name.pathName, name)
        case _ => State.pure(())
      }

  override def constantDefined(name: Name[S]): SX[Option[Type]] =
    getState.map(_.constants.get(name.value))

  def readArrow(name: Name[S]): SX[Option[ArrowType]] =
    readArrowHelper(name.value).flatMap {
      case Some(at) =>
        locations.pointLocation(name.pathName, name).as(at.some)
      case None =>
        // check if we have arrow in variable
        readName(name.value).flatMap {
          case Some(at @ ArrowType(_, _)) =>
            locations.pointLocation(name.pathName, name).as(at.some)
          case _ =>
            getState.flatMap(st =>
              report
                .error(
                  name,
                  Levenshtein.genMessage(
                    s"Name '${name.value}' not found in scope",
                    name.value,
                    st.allNames.toList
                  )
                )
                .as(Option.empty[ArrowType])
            )
        }
    }

  private def readArrowHelper(name: String): SX[Option[ArrowType]] =
    getState.map { st =>
      st.stack
        .flatMap(_.arrows.get(name))
        .headOption orElse st.rootArrows.get(name)
    }

  def defineInternal(name: String, `type`: Type): SX[Boolean] = {
    // is is for internal names definition, all errors are unexpected
    readName(name).flatMap {
      case Some(_) =>
        internalError(s"Unexpected error. Name $name was already defined")
      case None =>
        mapStackHeadM(
          internalError(s"Unexpected error. Cannot define $name in the root scope")
        )(fr => (fr.addInternalName(name, `type`) -> true).pure)
    }
  }

  override def define(name: Name[S], `type`: Type): SX[Boolean] =
    readName(name.value).flatMap {
      case Some(_) =>
        getState.map(_.definitions.get(name.value).exists(_ == name)).flatMap {
          case true => State.pure(false)
          case false => report.error(name, "This name was already defined in the scope").as(false)
        }
      case None =>
        mapStackHeadM(report.error(name, "Cannot define a variable in the root scope").as(false))(
          fr => (fr.addName(name, `type`) -> true).pure
        ) <* locations.addDefinition(DefinitionInfo(name.pathName, name, `type`))
    }

  override def derive(name: Name[S], `type`: Type, derivedFrom: Set[String]): State[X, Boolean] =
    define(name, `type`).flatTap(defined =>
      mapStackHead_(_.derived(name, derivedFrom)).whenA(defined)
    )

  override def getDerivedFrom(fromNames: List[Set[String]]): State[X, List[Set[String]]] =
    mapStackHead(Nil)(frame =>
      frame -> fromNames.map(ns =>
        frame.derivedFrom.view.filterKeys(ns).values.toList.combineAll ++ ns
      )
    )

  override def defineConstant(name: Name[S], `type`: Type): SX[Boolean] =
    readName(name.value).flatMap {
      case Some(_) =>
        report.error(name, "This name was already defined in the scope").as(false)
      case None =>
        modify(st =>
          st.copy(
            constants = st.constants.updated(name.value, `type`)
          )
        ).as(true)
    } <* locations.addDefinition(DefinitionInfo(name.pathName, name, `type`))

  override def defineArrow(name: Name[S], arrowType: ArrowType, isRoot: Boolean): SX[Boolean] =
    readName(name.value).flatMap {
      case Some(_) =>
        getState.map(_.definitions.get(name.value).exists(_ == name)).flatMap {
          case true => State.pure(false)
          case false => report.error(name, "This arrow was already defined in the scope").as(false)
        }

      case None =>
        mapStackHeadM(
          if (isRoot)
            modify(st =>
              st.copy(
                rootArrows = st.rootArrows.updated(name.value, arrowType),
                definitions = st.definitions.updated(name.value, name)
              )
            )
              .as(true)
          else
            report
              .error(name, "Cannot define a variable in the root scope")
              .as(false)
        )(fr => (fr.addArrow(name, arrowType) -> true).pure) <*
          locations.addDefinition(DefinitionInfo[S](name.pathName, name, arrowType))
    }

  override def beginScope(token: Token[S]): SX[Unit] =
    stackInt.beginScope(NamesState.Frame(token))

  override def endScope(): SX[Unit] = stackInt.endScope
}
