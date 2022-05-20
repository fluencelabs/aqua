package aqua.semantics

import aqua.raw.Raw
import aqua.raw.RawContext
import aqua.semantics.rules.abilities.AbilitiesState
import aqua.semantics.rules.names.NamesState
import aqua.semantics.rules.types.TypesState
import cats.Semigroup
import cats.data.{Chain, State}
import cats.kernel.Monoid
import cats.syntax.monoid.*

case class CompilerState[S[_]](
  errors: Chain[SemanticError[S]] = Chain.empty[SemanticError[S]],
  names: NamesState[S] = NamesState[S](),
  abilities: AbilitiesState[S] = AbilitiesState[S](),
  types: TypesState[S] = TypesState[S]()
)

object CompilerState {
  type St[S[_]] = State[CompilerState[S], Raw]

  def init[F[_]](ctx: RawContext): CompilerState[F] =
    CompilerState(
      names = NamesState.init[F](ctx),
      abilities = AbilitiesState.init[F](ctx),
      types = TypesState.init[F](ctx)
    )

  implicit def semigroupState[S[_]]: Semigroup[CompilerState[S]] =
    (x: CompilerState[S], y: CompilerState[S]) => {
      CompilerState[S](
        x.errors ++ y.errors,
        x.names |+| y.names,
        x.abilities |+| y.abilities,
        x.types |+| y.types
      )
    }

  implicit def compilerStateMonoid[S[_]]: Monoid[St[S]] = new Monoid[St[S]] {
    override def empty: St[S] = State.pure(Raw.Empty("compiler state monoid empty"))

    override def combine(x: St[S], y: St[S]): St[S] = for {
      a <- x.get
      b <- y.get
      _ <- State.set(
        CompilerState[S](
          a.errors ++ b.errors,
          a.names |+| b.names,
          a.abilities |+| b.abilities,
          a.types |+| b.types
        )
      )
      am <- x
      ym <- y
    } yield {
      // println(s"MONOID COMBINE $am $ym")
      am |+| ym
    }
  }

}
