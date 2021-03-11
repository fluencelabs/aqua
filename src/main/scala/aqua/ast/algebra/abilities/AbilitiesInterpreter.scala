package aqua.ast.algebra.abilities

import aqua.ast.algebra.types.ArrowType
import aqua.parser.lexer.{Name, Token}
import cats.data.State
import cats.~>
import shapeless.Lens

class AbilitiesInterpreter[F[_], X](implicit lens: Lens[X, AbState[F]]) extends (AbilityOp.Aux[F, *] ~> State[X, *]) {

  private def getState: State[X, AbState[F]] = State.get.map(lens.get)
  private def setState(st: AbState[F]): State[X, Unit] = State.modify(s => lens.set(s)(st))
  private def modify(f: AbState[F] => AbState[F]): State[X, Unit] = State.modify(s => lens.set(s)(f(lens.get(s))))

  override def apply[A](fa: AbilityOp.Aux[F, A]): State[X, A] =
    (fa match {
      case bs: BeginScope[F] =>
        modify(_.beginScope(bs.token))
      case EndScope() =>
        modify(_.endScope)

      case PurgeArrows() =>
        for {
          st <- getState
          // get arrows
          // if empty, error
          // otherwise, clean and return
        } yield ()

      case GetArrow(name, arrow) =>
      // Find the scope with ability
      // get ability arrows
      // find arrow by name
      // if no matching arrow, error
      case SetServiceId(name, id) =>
      // in current scope, set service id by its name
      // check that it's registered, and that it is a service
      case DefineArrow(arrow, t) =>
      // in current scope, save arrow in the cache
      // if an arrow with this name already exists, raise
      case DefineService(name, arrows) =>
      // in current scope, define a service (or do it globally?)
      // in case service name is already used for another ability, raise
    }).asInstanceOf[State[X, A]]
}

case class AbState[F[_]](stack: List[AbScope[F]]) {
  def beginScope(token: Token[F]): AbState[F] = copy[F](AbScope[F](token) :: stack)
  def endScope: AbState[F] = copy[F](stack.tail)
}

case class AbScope[F[_]](
  token: Token[F],
  arrows: Map[String, (Name[F], ArrowType)] = Map.empty[String, (Name[F], ArrowType)]
)
