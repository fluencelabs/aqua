package aqua.ast.algebra.abilities

import aqua.AquaError
import aqua.ast.algebra.types.ArrowType
import aqua.parser.lexer.{Name, Token}
import cats.data.{EitherT, NonEmptyList, NonEmptyMap, State}
import cats.~>
import shapeless.Lens
import cats.syntax.functor._

class AbilitiesInterpreter[F[_], X](implicit lens: Lens[X, AbState[F]]) extends (AbilityOp.Aux[F, *] ~> State[X, *]) {

  type S[A] = State[X, A]

  private def getState: S[AbState[F]] = State.get.map(lens.get)
  private def setState(st: AbState[F]): S[Unit] = State.modify(s => lens.set(s)(st))

  private def modify(f: AbState[F] => AbState[F]): S[Unit] =
    State.modify(s => lens.set(s)(f(lens.get(s))))

  override def apply[A](fa: AbilityOp.Aux[F, A]): State[X, A] =
    (fa match {
      case bs: BeginScope[F] =>
        modify(_.beginScope(bs.token))
      case EndScope() =>
        modify(_.endScope)

      case PurgeArrows() =>
        getState.map(_.purgeArrows).flatMap {
          case (Some(arrs), nextState) => setState(nextState).as(arrs)
          case _ => ??? //setError
        }

      case GetArrow(name, arrow) =>
        // Find the scope with ability
        // get ability arrows
        // find arrow by name
        // if no matching arrow, error
        ???
      case SetServiceId(name, id) =>
      // in current scope, set service id by its name
      // check that it's registered, and that it is a service
      case DefineArrow(arrow, t) =>
        // in current scope, save arrow in the cache
        // if an arrow with this name already exists, raise
        ???

      case DefineService(name, arrows) =>
        // in current scope, define a service (or do it globally?)
        // in case service name is already used for another ability, raise
        ???

    }).asInstanceOf[State[X, A]]
}

case class AbState[F[_]](stack: List[AbScope[F]]) {
  def beginScope(token: Token[F]): AbState[F] = copy[F](AbScope[F](token) :: stack)
  def endScope: AbState[F] = copy[F](stack.tail)

  def purgeArrows: (Option[NonEmptyList[(Name[F], ArrowType)]], AbState[F]) =
    stack match {
      case sc :: tail =>
        NonEmptyList.fromList(sc.arrows.values.toList) -> copy[F](sc.copy(arrows = Map.empty) :: tail)
      case _ => None -> this
    }
}

case class AbScope[F[_]](
  token: Token[F],
  arrows: Map[String, (Name[F], ArrowType)] = Map.empty[String, (Name[F], ArrowType)]
)
