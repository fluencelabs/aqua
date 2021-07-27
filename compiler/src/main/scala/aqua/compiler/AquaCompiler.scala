package aqua.compiler

import aqua.backend.Backend
import aqua.linker.Linker
import aqua.model.AquaContext
import aqua.model.transform.BodyConfig
import aqua.parser.lift.LiftParser
import aqua.semantics.Semantics
import cats.{Comonad, Monad}
import cats.data.{Chain, NonEmptyChain, Validated, ValidatedNec}
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.applicative._
import cats.syntax.traverse._

object AquaCompiler {

  def compile[F[_]: Monad, E, I, S[_]: Comonad](
    sources: AquaSources[F, E, I],
    liftI: I => LiftParser[S],
    backend: Backend,
    config: BodyConfig
  ): F[ValidatedNec[AquaError[I, E, S], Chain[AquaCompiled[I]]]] = {
    import config.aquaContextMonoid
    type Err = AquaError[I, E, S]
    new AquaParser[F, E, I, S](sources, liftI)
      .resolve[ValidatedNec[Err, AquaContext]](ast =>
        context =>
          context.andThen(ctx => Semantics.process(ast, ctx).leftMap(_.map[Err](CompileError(_))))
      )
      .map {
        case Validated.Valid(modules) =>
          Linker[I, AquaError[I, E, S], ValidatedNec[Err, AquaContext]](
            modules,
            cycle => CycleError[I, E, S](cycle.map(_.id))
          ) match {
            case Validated.Valid(files) =>
              files
                .foldLeft[ValidatedNec[Err, Chain[AquaProcessed[I]]]](
                  Validated.validNec(Chain.nil)
                ) {
                  case (acc, (i, Validated.Valid(res))) =>
                    acc combine Validated.validNec(Chain.one(AquaProcessed(i, res)))
                  case (acc, (_, Validated.Invalid(errs))) =>
                    acc combine Validated.invalid(errs)
                }
                .map(
                  _.flatMap(ap =>
                    Chain fromSeq backend
                      .generate(ap.context, config)
                      .map(AquaCompiled(ap.id, _))
                  )
                )

            case Validated.Invalid(errs) =>
              Validated.invalid(errs)
          }
        case Validated.Invalid(errs) =>
          Validated.invalid(errs)
      }
  }

  def compileTo[F[_]: Monad, E, I, S[_]: Comonad, T](
    sources: AquaSources[F, E, I],
    liftI: I => LiftParser[S],
    backend: Backend,
    config: BodyConfig,
    write: AquaCompiled[I] => F[Validated[E, T]]
  ): F[ValidatedNec[AquaError[I, E, S], Chain[T]]] =
    compile[F, E, I, S](sources, liftI, backend, config).flatMap {
      case Validated.Valid(compiled) =>
        compiled
          .map(ac =>
            write(ac).map(
              _.bimap[NonEmptyChain[AquaError[I, E, S]], Chain[T]](
                e => NonEmptyChain.one(OutputError(ac, e)),
                Chain.one
              )
            )
          )
          .toList
          .traverse(identity)
          .map(
            _.foldLeft[ValidatedNec[AquaError[I, E, S], Chain[T]]](Validated.validNec(Chain.nil))(
              _ combine _
            )
          )

      case Validated.Invalid(errs) =>
        Validated.invalid[NonEmptyChain[AquaError[I, E, S]], Chain[T]](errs).pure[F]
    }
}
