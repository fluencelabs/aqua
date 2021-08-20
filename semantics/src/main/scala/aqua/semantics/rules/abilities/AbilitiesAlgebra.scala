package aqua.semantics.rules.abilities

import aqua.model.ValueModel
import aqua.parser.lexer.{Ability, Name, Token, Value}
import aqua.types.ArrowType
import cats.InjectK
import cats.data.{NonEmptyList, NonEmptyMap}
import cats.free.Free

class AbilitiesAlgebra[F[_], Alg[_]](implicit A: InjectK[AbilityOp[F, *], Alg]) {

  def defineArrow(arrow: Name[F], `type`: ArrowType): Free[Alg, Boolean] =
    Free.liftInject[Alg](DefineArrow[F](arrow, `type`))

  def purgeArrows(token: Token[F]): Free[Alg, Option[NonEmptyList[(Name[F], ArrowType)]]] =
    Free.liftInject[Alg](PurgeArrows[F](token))

  def defineService(
    name: Ability[F],
    arrows: NonEmptyMap[String, ArrowType],
    defaultId: Option[ValueModel]
  ): Free[Alg, Boolean] =
    Free.liftInject[Alg](DefineService[F](name, arrows, defaultId))

  def getArrow(name: Ability[F], arrow: Name[F]): Free[Alg, Option[ArrowType]] =
    Free.liftInject[Alg](GetArrow[F](name, arrow))

  def setServiceId(name: Ability[F], id: Value[F], vm: ValueModel): Free[Alg, Boolean] =
    Free.liftInject[Alg](SetServiceId[F](name, id, vm))

  def getServiceId(name: Ability[F]): Free[Alg, Either[Boolean, ValueModel]] =
    Free.liftInject[Alg](GetServiceId[F](name))

  def beginScope(token: Token[F]): Free[Alg, Unit] =
    Free.liftInject[Alg](BeginScope[F](token))

  def endScope(): Free[Alg, Boolean] =
    Free.liftInject[Alg](EndScope[F]())

}

object AbilitiesAlgebra {

  implicit def abilitiesAlgebra[F[_], Alg[_]](implicit
    A: InjectK[AbilityOp[F, *], Alg]
  ): AbilitiesAlgebra[F, Alg] =
    new AbilitiesAlgebra[F, Alg]()
}
