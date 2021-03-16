package aqua.ast.algebra.names

import aqua.ast.algebra.types.{ArrowType, Type}
import aqua.ast.algebra.{ReportError, StackInterpreter}
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
      } orElse st.rootNames.get(name)
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
        readName(ra.name.value).flatMap {
          case Some(t: ArrowType) =>
            State.pure(Option(t))
          case Some(t) =>
            report(ra.name, s"Arrow type expected, got: $t").as(Option.empty[ArrowType])
          case None =>
            getState.flatMap(st =>
              report(ra.name, "Undefined name, available: " + st.allNames.mkString(", ")).as(Option.empty[ArrowType])
            )

        }
      case dn: DefineName[F] =>
        readName(dn.name.value).flatMap {
          case Some(_) => report(dn.name, "This name was already defined in the scope").as(false)
          case None =>
            mapStackHead(
              if (dn.isRoot)
                modify(st => st.copy(rootNames = st.rootNames.updated(dn.name.value, dn.`type`)))
                  .as(true)
              else
                report(dn.name, "Cannot define a variable in the root scope")
                  .as(false)
            )(fr => fr.addName(dn.name.value, dn.`type`) -> true)
        }
      case bs: BeginScope[F] =>
        beginScope(NamesFrame(bs.token))
      case _: EndScope[F] =>
        endScope
    }).asInstanceOf[State[X, A]]
}

case class NamesState[F[_]](stack: List[NamesFrame[F]] = Nil, rootNames: Map[String, Type] = Map.empty) {
  def allNames: LazyList[String] = LazyList.from(stack).flatMap(_.names.keys).appendedAll(rootNames.keys)
}

case class NamesFrame[F[_]](token: Token[F], names: Map[String, Type] = Map.empty) {
  def addName(n: String, t: Type): NamesFrame[F] = copy[F](names = names.updated(n, t))
}
