package aqua.semantics

import aqua.model.{AquaContext, EmptyModel, Model, VarModel}
import aqua.semantics.rules.abilities.AbilitiesState
import aqua.semantics.rules.names.NamesState
import aqua.semantics.rules.types.TypesState
import cats.data.{Chain, State}
import cats.kernel.Monoid
import cats.syntax.monoid._

case class CompilerState[F[_]](
  errors: Chain[SemanticError[F]] = Chain.empty[SemanticError[F]],
  names: NamesState[F] = NamesState[F](),
  abilities: AbilitiesState[F] = AbilitiesState[F](),
  types: TypesState[F] = TypesState[F]()
)

object CompilerState {
  type S[F[_]] = State[CompilerState[F], Model]

  def init[F[_]](ctx: AquaContext): CompilerState[F] = {
    // TODO: should go to Monoid[AquaContext].empty, along with overriden constants
    val withLE = ctx.copy(values = ctx.values + ("%last_error%" -> VarModel.lastError))
    CompilerState(
      names = NamesState.init[F](withLE),
      abilities = AbilitiesState.init[F](withLE),
      types = TypesState.init[F](withLE)
    )
  }

  implicit def compilerStateMonoid[F[_]]: Monoid[S[F]] = new Monoid[S[F]] {
    override def empty: S[F] = State.pure(EmptyModel("compiler state monoid empty"))

    override def combine(x: S[F], y: S[F]): S[F] = for {
      a <- x.get
      b <- y.get
      _ <- State.set(
        CompilerState[F](
          a.errors ++ b.errors,
          a.names |+| b.names,
          a.abilities |+| b.abilities,
          a.types |+| b.types
        )
      )
      am <- x
      ym <- y
    } yield {
      //println(s"MONOID COMBINE $am $ym")
      am |+| ym
    }
  }

}
