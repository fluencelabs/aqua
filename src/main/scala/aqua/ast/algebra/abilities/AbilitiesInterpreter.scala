package aqua.ast.algebra.abilities

import aqua.ast.algebra.ReportError
import aqua.ast.algebra.types.ArrowType
import aqua.parser.lexer.{Name, Token, Value}
import cats.data.{NonEmptyList, NonEmptyMap, State}
import cats.{~>, Comonad}
import shapeless.Lens
import cats.syntax.functor._
import cats.syntax.comonad._

class AbilitiesInterpreter[F[_]: Comonad, X](implicit lens: Lens[X, AbState[F]], error: ReportError[F, X])
    extends (AbilityOp[F, *] ~> State[X, *]) {

  type S[A] = State[X, A]

  private def getState: S[AbState[F]] = State.get.map(lens.get)
  private def setState(st: AbState[F]): S[Unit] = State.modify(s => lens.set(s)(st))

  private def report(t: Token[F], hint: String): S[Unit] =
    State.modify(error(_, t, hint))

  private def modify(f: AbState[F] => AbState[F]): S[Unit] =
    State.modify(s => lens.set(s)(f(lens.get(s))))

  override def apply[A](fa: AbilityOp[F, A]): State[X, A] =
    (fa match {
      case bs: BeginScope[F] =>
        modify(_.beginScope(bs.token))

      case EndScope() =>
        modify(_.endScope)

      case pa: PurgeArrows[F] =>
        getState.map(_.purgeArrows).flatMap {
          case Some((arrs, nextState)) => setState(nextState).as(Option[NonEmptyList[(Name[F], ArrowType)]](arrs))
          case _ =>
            report(pa.token, "Cannot purge arrows, no arrows provided")
              .as(Option.empty[NonEmptyList[(Name[F], ArrowType)]])
        }

      case ga: GetArrow[F] =>
        getState.map(_.services.get(ga.name.name.extract)).flatMap {
          case Some(arrows) =>
            arrows(ga.arrow.name.extract)
              .fold(
                report(
                  ga.arrow,
                  s"Service found, but arrow is undefined, available: ${arrows.value.keys.toNonEmptyList.toList.mkString(", ")}"
                ).as(Option.empty[ArrowType])
              )(a => State.pure(Some(a)))
          case None =>
            report(ga.name, "Ability with this name is undefined").as(Option.empty[ArrowType])
        }

      case s: SetServiceId[F] =>
        getState.map(_.services.get(s.name.name.extract)).flatMap {
          case Some(_) =>
            getState.map(_.stack).flatMap {
              case h :: tail =>
                modify(_.copy(stack = h.copy(serviceIds = h.serviceIds.updated(s.name.name.extract, s.id)) :: tail))
                  .as(true)

              case _ =>
                report(s.name, "Trying to set service ID while out of the scope").as(false)
            }
          case None =>
            report(s.name, "Service with this name is not registered, can't set its ID").as(false)
        }

      case da: DefineArrow[F] =>
        getState.map(_.stack).flatMap {
          case h :: tail =>
            h.arrows.get(da.arrow.name.extract) match {
              case Some(_) => report(da.arrow, "Arrow with this name was already defined above").as(false)
              case None =>
                modify(
                  _.copy(stack =
                    h.copy(arrows = h.arrows.updated(da.arrow.name.extract, da.arrow -> da.`type`)) :: tail
                  )
                ).as(true)
            }
          case _ =>
            report(da.arrow, "No abilities definition scope is found").as(false)
        }

      case ds: DefineService[F] =>
        getState.map(_.services.get(ds.name.name.extract)).flatMap {
          case Some(_) => report(ds.name, "Service with this name was already defined").as(false)
          case None => modify(s => s.copy(services = s.services.updated(ds.name.name.extract, ds.arrows))).as(true)
        }

    }).asInstanceOf[State[X, A]]
}

case class AbState[F[_]](
  stack: List[AbScope[F]] = Nil,
  services: Map[String, NonEmptyMap[String, ArrowType]] = Map.empty
) {
  def beginScope(token: Token[F]): AbState[F] = copy[F](AbScope[F](token) :: stack)
  def endScope: AbState[F] = copy[F](stack.tail)

  def purgeArrows: Option[(NonEmptyList[(Name[F], ArrowType)], AbState[F])] =
    stack match {
      case sc :: tail =>
        NonEmptyList.fromList(sc.arrows.values.toList).map(_ -> copy[F](sc.copy(arrows = Map.empty) :: tail))
      case _ => None
    }
}

case class AbScope[F[_]](
  token: Token[F],
  arrows: Map[String, (Name[F], ArrowType)] = Map.empty[String, (Name[F], ArrowType)],
  serviceIds: Map[String, Value[F]] = Map.empty[String, Value[F]]
)
