package aqua.interim.abilities

import aqua.AquaError
import aqua.interim.types.ArrowType
import aqua.parser.lexer.{Name, Token}
import cats.data.{State, Writer}
import cats.~>
import shapeless.Lens

class AbilitiesInterpreter[F[_], X](implicit lens: Lens[X, AbState[F]], error: Writer[X, AquaError])
    extends (AbilityOp ~> State[X, *]) {

  private def getState: State[X, AbState[F]] = State.get.map(lens.get)
  private def setState(st: AbState[F]): State[X, Unit] = State.modify(s => lens.set(s)(st))
  private def modify(f: AbState[F] => AbState[F]): State[X, Unit] = State.modify(s => lens.set(s)(f(lens.get(s))))

  override def apply[A](fa: AbilityOp[A]): State[X, A] =
    fa match {
      case BeginScope(token: Token[F]) =>
        modify(_.beginScope(token))
      case EndScope() =>
        modify(_.endScope)

      case PurgeArrows() =>
        for {
          st <- getState
        } yield ()

      case UnsetServiceId(name) =>
      case GetArrow(name, arrow) =>
      case SetServiceId(name, id) =>
      case DefineArrow(arrow, t) =>
      case DefineService(name, arrows) =>
    }
}

case class AbState[F[_]](stack: List[AbScope[F]]) {
  def beginScope(token: Token[F]): AbState[F] = copy[F](AbScope(token) :: stack)
  def endScope: AbState[F] = copy[F](stack.tail)
}

case class AbScope[F[_]](token: Token[F], arrows: Map[String, (Name[F], ArrowType)] = Map.empty)
