package aqua.semantics.rules.names

import aqua.parser.lexer.{Name, Token}
import aqua.semantics.lsp.{TokenArrowInfo, TokenType, TokenTypeInfo}
import aqua.semantics.Levenshtein
import aqua.semantics.rules.locations.LocationsAlgebra
import aqua.semantics.rules.{ReportError, StackInterpreter}
import aqua.types.{ArrowType, StreamType, Type}
import cats.data.{OptionT, State}
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.~>
import monocle.Lens
import monocle.macros.GenLens

class NamesInterpreter[S[_], X](implicit
  lens: Lens[X, NamesState[S]],
  error: ReportError[S, X],
  locations: LocationsAlgebra[S, State[X, *]]
) extends NamesAlgebra[S, State[X, *]] {

  val stackInt = new StackInterpreter[S, X, NamesState[S], NamesState.Frame[S]](
    GenLens[NamesState[S]](_.stack)
  )

  import stackInt.{getState, mapStackHead, modify, report}

  type SX[A] = State[X, A]

  def readName(name: String): SX[Option[TokenType[S]]] =
    getState.map { st =>
      st.constants.get(name) orElse st.stack.collectFirst {
        case frame if frame.names.contains(name) => frame.names(name)
        case frame if frame.arrows.contains(name) => frame.arrows(name)
      } orElse st.rootArrows.get(name)
    }

  override def read(name: Name[S], mustBeDefined: Boolean = true): SX[Option[Type]] =
    OptionT(constantInfo(name))
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
        case Some(tokenInfo) =>
          locations.addNameLocation(name, tokenInfo)
        case _ => State.pure(())
      }
      .map(_.map(_.tokenType))

  def constantInfo(name: Name[S]): SX[Option[TokenType[S]]] =
    getState.map(_.constants.get(name.value))

  override def constantDefined(name: Name[S]): SX[Option[Type]] =
    constantInfo(name).map(_.map(_.tokenType))

  def readArrow(name: Name[S]): SX[Option[ArrowType]] =
    readArrowHelper(name.value).flatMap {
      case Some(g) =>
        locations.addNameLocation(name, g).map(_ => Option(g.tokenType))
      case None =>
        // check if we have arrow in variable
        readName(name.value).flatMap {
          case Some(tt @ TokenTypeInfo(_, at @ ArrowType(_, _))) =>
            locations.addNameLocation(name, tt).map(_ => Option(at))
          case _ =>
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
    }

  def readArrowHelper(name: String): SX[Option[TokenArrowInfo[S]]] =
    getState.map { st =>
      st.stack
        .flatMap(_.arrows.get(name))
        .headOption orElse st.rootArrows.get(name)
    }

  override def define(name: Name[S], `type`: Type): SX[Boolean] =
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
        )(fr => fr.addName(name, `type`) -> true)
    }

  override def derive(name: Name[S], `type`: Type, derivedFrom: Set[String]): State[X, Boolean] =
    define(name, `type`).flatMap {
      case true =>
        mapStackHead(State.pure(true))(_.derived(name, derivedFrom) -> true)
      case false => State.pure(false)
    }

  override def getDerivedFrom(fromNames: List[Set[String]]): State[X, List[Set[String]]] =
    mapStackHead(State.pure(Nil))(fr =>
      fr -> fromNames.map(ns => fr.derivedFrom.view.filterKeys(ns).values.foldLeft(ns)(_ ++ _))
    )

  override def defineConstant(name: Name[S], `type`: Type): SX[Boolean] =
    readName(name.value).flatMap {
      case Some(_) =>
        report(name, "This name was already defined in the scope").as(false)
      case None =>
        modify(st =>
          st.copy(
            constants = st.constants.updated(name.value, TokenTypeInfo(Some(name), `type`))
          )
        ).as(true)
    }

  override def defineArrow(name: Name[S], gen: ArrowType, isRoot: Boolean): SX[Boolean] =
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
                rootArrows = st.rootArrows.updated(name.value, TokenArrowInfo(Some(name), gen)),
                definitions = st.definitions.updated(name.value, name)
              )
            )
              .as(true)
          else
            report(name, "Cannot define a variable in the root scope")
              .as(false)
        )(fr => fr.addArrow(name, gen) -> true)
    }

  override def beginScope(token: Token[S]): SX[Unit] =
    stackInt.beginScope(NamesState.Frame(token))

  override def streamsDefinedWithinScope(): SX[Map[String, StreamType]] =
    stackInt.mapStackHead(State.pure(Map.empty[String, StreamType])) { frame =>
      frame -> frame.names.collect { case (n, TokenTypeInfo(_, st @ StreamType(_))) =>
        n -> st
      }
    }

  override def endScope(): SX[Unit] = stackInt.endScope

}
