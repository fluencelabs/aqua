package aqua.ast.algebra.names

import aqua.ast.algebra.{ReportError, StackInterpreter}
import cats.data.State
import cats.~>
import monocle.Lens
import monocle.macros.GenLens

class NamesInterpreter[F[_], X](implicit lens: Lens[X, NamesState[F]], error: ReportError[F, X])
    extends StackInterpreter[F, X, NamesState[F], NamesFrame[F]](GenLens[NamesState[F]](_.stack))
    with (NameOp[F, *] ~> State[X, *]) {

  override def apply[A](fa: NameOp[F, A]): State[X, A] = ???
}

case class NamesState[F[_]](stack: List[NamesFrame[F]])

case class NamesFrame[F[_]]()
