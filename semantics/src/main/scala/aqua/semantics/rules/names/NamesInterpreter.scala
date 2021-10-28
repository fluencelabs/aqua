package aqua.semantics.rules.names

import aqua.parser.lexer.{Name, Token}
import aqua.semantics.Levenshtein
import aqua.semantics.rules.{ReportError, StackInterpreter}
import aqua.types.{ArrowType, Type}
import cats.data.{OptionT, State}
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.~>
import monocle.Lens
import monocle.macros.GenLens

class NamesInterpreter[F[_], X](implicit lens: Lens[X, NamesState[F]], error: ReportError[F, X])
    extends NamesAlgebra[F, State[X, *]] {

  val stackInt = new StackInterpreter[F, X, NamesState[F], NamesState.Frame[F]](
    GenLens[NamesState[F]](_.stack)
  )

  import stackInt.{report, modify, mapStackHead, getState}

  type SN[A] = State[X, A]

  def readName(name: String): SN[Option[Type]] =
    getState.map { st =>
      st.constants.get(name) orElse st.stack.collectFirst {
        case frame if frame.names.contains(name) => frame.names(name)
        case frame if frame.arrows.contains(name) => frame.arrows(name)
      } orElse st.rootArrows.get(name)
    }

  override def read(name: Name[F], mustBeDefined: Boolean = true): SN[Option[Type]] =
    OptionT(constantDefined(name))
      .orElseF(readName(name.value))
      .value
      .flatTap {
        case None if mustBeDefined =>
          getState.flatMap(st =>
            report(
              name,
              Levenshtein
                .genMessage(
                  s"Name '${name.value}' isn't found in scope",
                  name.value,
                  st.allNames.toList
                )
            )
          )
        case _ => State.pure(())
      }

  override def constantDefined(name: Name[F]): SN[Option[Type]] =
    getState.map(_.constants.get(name.value))

  def readArrow(name: Name[F]): SN[Option[ArrowType]] =
    readArrowHelper(name.value).flatMap {
      case Some(g) => State.pure(Option(g))
      case None =>
        getState.flatMap(st =>
          report(
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

  def readArrowHelper(name: String): SN[Option[ArrowType]] =
    getState.map { st =>
      st.stack.flatMap(_.arrows.get(name)).headOption orElse st.rootArrows.get(name)
    }

  override def define(name: Name[F], `type`: Type): SN[Boolean] =
    readName(name.value).flatMap {
      case Some(_) =>
        getState.map(_.definitions.get(name.value).exists(_ == name)).flatMap {
          case true => State.pure(false)
          case false => report(name, "This name was already defined in the scope").as(false)
        }
      case None =>
        mapStackHead(
          report(name, "Cannot define a variable in the root scope")
            .as(false)
        )(fr => fr.addName(name.value, `type`) -> true)
    }

  override def defineConstant(name: Name[F], `type`: Type): SN[Boolean] =
    readName(name.value).flatMap {
      case Some(_) =>
        report(name, "This name was already defined in the scope").as(false)
      case None =>
        modify(st =>
          st.copy(
            constants = st.constants.updated(name.value, `type`)
          )
        ).as(true)
    }

  override def defineArrow(name: Name[F], gen: ArrowType, isRoot: Boolean): SN[Boolean] =
    readName(name.value).flatMap {
      case Some(_) =>
        getState.map(_.definitions.get(name.value).exists(_ == name)).flatMap {
          case true => State.pure(false)
          case false => report(name, "This arrow was already defined in the scope").as(false)
        }

      case None =>
        mapStackHead(
          if (isRoot)
            modify(st =>
              st.copy(
                rootArrows = st.rootArrows.updated(name.value, gen),
                definitions = st.definitions.updated(name.value, name)
              )
            )
              .as(true)
          else
            report(name, "Cannot define a variable in the root scope")
              .as(false)
        )(fr => fr.addArrow(name.value, gen) -> true)
    }

  override def beginScope(token: Token[F]): SN[Unit] =
    stackInt.beginScope(NamesState.Frame(token))

  override def endScope(): SN[Unit] = stackInt.endScope

}
