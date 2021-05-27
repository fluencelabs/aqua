package aqua.semantics

import aqua.model.func.FuncModel
import aqua.model._
import aqua.parser.lexer.Token
import aqua.semantics.rules.abilities.AbilitiesState
import aqua.semantics.rules.names.NamesState
import aqua.semantics.rules.types.TypesState
import aqua.types.ArrowType
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

  def fromModels[F[_]](models: Chain[Model]): CompilerState[F] = {
    models.foldLeft(CompilerState[F]()) { case (cs, model) =>
      model match {
        case ConstantModel(name, value) =>
          cs.copy(names = cs.names |+| NamesState[F](constants = Map(name -> value.`type`)))
        case FuncModel(name, args, ret, _) =>
          cs.copy(names =
            cs.names |+| NamesState[F](
              rootArrows = Map(name -> ArrowType(args.types, ret.map(_._2))),
              definitions = Set(name)
            )
          )
        case sm @ ServiceModel(name, _, serviceId) =>
          cs.copy(abilities =
            cs.abilities |+| AbilitiesState(
              services = Map(name -> sm),
              rootServiceIds =
                serviceId.fold[Map[String, ValueModel]](Map.empty)(id => Map(name -> id))
            )
          )
        case _ => cs
      }

    }
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
