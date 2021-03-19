package aqua.semantics.algebra.scope

import aqua.semantics.algebra.{ReportError, StackInterpreter}
import aqua.parser.lexer.Value
import cats.data.State
import cats.~>
import monocle.Lens
import monocle.macros.GenLens

class PeerIdInterpreter[F[_], X](implicit lens: Lens[X, PeerIdState[F]], error: ReportError[F, X])
    extends StackInterpreter[F, X, PeerIdState[F], Value[F]](GenLens[PeerIdState[F]](_.stack))
    with (PeerIdOp[F, *] ~> State[X, *]) {

  override def apply[A](fa: PeerIdOp[F, A]): State[X, A] =
    (fa match {
      case CurrentPeerId() =>
        getState.map(_.stack.headOption)
      case _: ErasePeerId[F] =>
        endScope
      case opi: OnPeerId[F] =>
        beginScope(opi.id)
    }).asInstanceOf[State[X, A]]
}

case class PeerIdState[F[_]](stack: List[Value[F]] = Nil)
