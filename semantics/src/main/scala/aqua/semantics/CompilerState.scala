package aqua.semantics

import aqua.model.{EmptyModel, Model}
import aqua.parser.lexer.Token
import aqua.semantics.rules.abilities.AbilitiesState
import aqua.semantics.rules.names.NamesState
import aqua.semantics.rules.types.TypesState
import cats.data.{Chain, State}
import cats.kernel.Monoid
import cats.syntax.monoid._

case class CompilerState[F[_]](
  errors: Chain[(Token[F], String)] = Chain.empty[(Token[F], String)],
  names: NamesState[F] = NamesState[F](),
  abilities: AbilitiesState[F] = AbilitiesState[F](),
  types: TypesState[F] = TypesState[F]()
)

object CompilerState {
  type S[F[_]] = State[CompilerState[F], Model]

  implicit def compilerStateMonoid[F[_]]: Monoid[S[F]] = new Monoid[S[F]] {
    override def empty: S[F] = State.pure(EmptyModel("compiler state monoid empty"))

    override def combine(x: S[F], y: S[F]): S[F] = for {
      a <- x.get
      b <- y.get
      _ <- State.set(
        CompilerState[F](
          a.errors ++ b.errors,
          NamesState(Nil, a.names.rootArrows ++ b.names.rootArrows),
          AbilitiesState(
            Nil,
            a.abilities.services ++ b.abilities.services,
            a.abilities.rootServiceIds ++ b.abilities.rootServiceIds
          ),
          TypesState(strict = a.types.strict ++ b.types.strict)
        )
      )
      am <- x
      ym <- y
    } yield {
      println(s"MONOID COMBINE $am $ym")
      am |+| ym
    }
  }

}
