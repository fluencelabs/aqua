package aqua.ast.algebra.names

import aqua.ast.algebra.types.Type
import aqua.ast.algebra.{ReportError, StackInterpreter}
import aqua.ast.gen.ArrowGen
import aqua.parser.lexer.Token
import cats.data.State
import cats.~>
import monocle.Lens
import monocle.macros.GenLens
import cats.syntax.functor._
import cats.syntax.flatMap._

class NamesInterpreter[F[_], X](implicit lens: Lens[X, NamesState[F]], error: ReportError[F, X])
    extends StackInterpreter[F, X, NamesState[F], NamesFrame[F]](GenLens[NamesState[F]](_.stack))
    with (NameOp[F, *] ~> State[X, *]) {

  def readName(name: String): S[Option[Type]] =
    getState.map { st =>
      st.stack.collectFirst {
        case frame if frame.names.contains(name) => frame.names(name)
        case frame if frame.arrows.contains(name) => frame.arrows(name).`type`
      } orElse st.rootArrows.get(name).map(_.`type`)
    }

  def readArrow(name: String): S[Option[ArrowGen]] =
    getState.map { st =>
      st.stack.flatMap(_.arrows.get(name)).headOption orElse st.rootArrows.get(name)
    }

  override def apply[A](fa: NameOp[F, A]): State[X, A] =
    (fa match {
      case rn: ReadName[F] =>
        readName(rn.name.value).flatTap {
          case Some(_) => State.pure(())
          case None =>
            getState.flatMap(st => report(rn.name, "Undefined name, available: " + st.allNames.mkString(", ")))
        }
      case ra: ReadArrow[F] =>
        readArrow(ra.name.value).flatMap {
          case Some(g) => State.pure(Option(g))
          case None =>
            getState.flatMap(st =>
              report(ra.name, "Undefined arrow, available: " + st.allNames.mkString(", "))
                .as(Option.empty[ArrowGen])
            )
        }

      case dn: DefineName[F] =>
        readName(dn.name.value).flatMap {
          case Some(_) => report(dn.name, "This name was already defined in the scope").as(false)
          case None =>
            mapStackHead(
              report(dn.name, "Cannot define a variable in the root scope")
                .as(false)
            )(fr => fr.addName(dn.name.value, dn.`type`) -> true)
        }
      case da: DefineArrow[F] =>
        readName(da.name.value).flatMap {
          case Some(_) => report(da.name, "This name was already defined in the scope").as(false)
          case None =>
            mapStackHead(
              if (da.isRoot)
                modify(st => st.copy(rootArrows = st.rootArrows.updated(da.name.value, da.gen)))
                  .as(true)
              else
                report(da.name, "Cannot define a variable in the root scope")
                  .as(false)
            )(fr => fr.addArrow(da.name.value, da.gen) -> true)
        }
      case bs: BeginScope[F] =>
        beginScope(NamesFrame(bs.token))
      case _: EndScope[F] =>
        endScope
    }).asInstanceOf[State[X, A]]
}

case class NamesState[F[_]](stack: List[NamesFrame[F]] = Nil, rootArrows: Map[String, ArrowGen] = Map.empty) {

  def allNames: LazyList[String] =
    LazyList.from(stack).flatMap(s => s.names.keys ++ s.arrows.keys).appendedAll(rootArrows.keys)

  def allArrows: LazyList[String] =
    LazyList.from(stack).flatMap(_.arrows.keys).appendedAll(rootArrows.keys)
}

case class NamesFrame[F[_]](
  token: Token[F],
  names: Map[String, Type] = Map.empty,
  arrows: Map[String, ArrowGen] = Map.empty
) {
  def addName(n: String, t: Type): NamesFrame[F] = copy[F](names = names.updated(n, t))
  def addArrow(n: String, g: ArrowGen): NamesFrame[F] = copy[F](arrows = arrows.updated(n, g))
}
