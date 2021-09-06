package aqua.semantics.rules.names

import aqua.semantics.rules.{ReportError, StackInterpreter}
import aqua.types.{ArrowType, Type}
import cats.data.{OptionT, State}
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.~>
import monocle.Lens
import monocle.macros.GenLens

class NamesInterpreter[F[_], X](implicit lens: Lens[X, NamesState[F]], error: ReportError[F, X])
    extends StackInterpreter[F, X, NamesState[F], NamesState.Frame[F]](
      GenLens[NamesState[F]](_.stack)
    ) with (NameOp[F, *] ~> State[X, *]) {

  def readName(name: String): S[Option[Type]] =
    getState.map { st =>
      st.constants.get(name) orElse st.stack.collectFirst {
        case frame if frame.names.contains(name) => frame.names(name)
        case frame if frame.arrows.contains(name) => frame.arrows(name)
      } orElse st.rootArrows.get(name)
    }

  def constantDefined(name: String): S[Option[Type]] =
    getState.map(_.constants.get(name))

  def readArrow(name: String): S[Option[ArrowType]] =
    getState.map { st =>
      st.stack.flatMap(_.arrows.get(name)).headOption orElse st.rootArrows.get(name)
    }

  override def apply[A](fa: NameOp[F, A]): State[X, A] =
    fa match {
      case rn: ReadName[F] =>
        OptionT(constantDefined(rn.name.value))
          .orElseF(readName(rn.name.value))
          .value
          .flatTap {
            case None if rn.mustBeDefined =>
              getState.flatMap(st =>
                report(rn.name, "Undefined name, available: " + st.allNames.mkString(", "))
              )
            case _ => State.pure(())
          }
      case rn: ConstantDefined[F] =>
        constantDefined(rn.name.value)

      case ra: ReadArrow[F] =>
        readArrow(ra.name.value).flatMap {
          case Some(g) => State.pure(Option(g))
          case None =>
            getState.flatMap(st =>
              report(ra.name, "Undefined arrow, available: " + st.allNames.mkString(", "))
                .as(Option.empty[ArrowType])
            )
        }

      case dc: DefineConstant[F] =>
        readName(dc.name.value).flatMap {
          case Some(_) =>
            report(dc.name, "This name was already defined in the scope").as(false)
          case None =>
            modify(st =>
              st.copy(
                constants = st.constants.updated(dc.name.value, dc.`type`)
              )
            ).as(true)
        }
      case dn: DefineName[F] =>
        readName(dn.name.value).flatMap {
          case Some(_) =>
            getState.map(_.definitions.get(dn.name.value).exists(_ == dn.name)).flatMap {
              case true => State.pure(false)
              case false => report(dn.name, "This name was already defined in the scope").as(false)
            }
          case None =>
            mapStackHead(
              report(dn.name, "Cannot define a variable in the root scope")
                .as(false)
            )(fr => fr.addName(dn.name.value, dn.`type`) -> true)
        }
      case da: DefineArrow[F] =>
        readName(da.name.value).flatMap {
          case Some(_) =>
            getState.map(_.definitions.get(da.name.value).exists(_ == da.name)).flatMap {
              case true => State.pure(false)
              case false => report(da.name, "This arrow was already defined in the scope").as(false)
            }

          case None =>
            mapStackHead(
              if (da.isRoot)
                modify(st =>
                  st.copy(
                    rootArrows = st.rootArrows.updated(da.name.value, da.gen),
                    definitions = st.definitions.updated(da.name.value, da.name)
                  )
                )
                  .as(true)
              else
                report(da.name, "Cannot define a variable in the root scope")
                  .as(false)
            )(fr => fr.addArrow(da.name.value, da.gen) -> true)
        }
      case bs: BeginScope[F] =>
        beginScope(NamesState.Frame(bs.token))
      case _: EndScope[F] =>
        endScope
    }
}
