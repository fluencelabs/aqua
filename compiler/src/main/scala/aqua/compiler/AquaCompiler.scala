package aqua.compiler

import aqua.backend.Backend
import aqua.linker.Linker
import aqua.model.AquaContext
import aqua.model.transform.BodyConfig
import aqua.parser.lift.LiftParser
import aqua.semantics.Semantics
import cats.data.Validated.{validNec, Invalid, Valid}
import cats.data.{Chain, NonEmptyChain, Validated, ValidatedNec}
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.{Comonad, Monad}

object AquaCompiler {

  def compile[F[_]: Monad, E, I, S[_]: Comonad](
    sources: AquaSources[F, E, I],
    liftI: (I, String) => LiftParser[S],
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
        case Valid(modules) =>
          Linker.link[I, AquaError[I, E, S], ValidatedNec[Err, AquaContext]](
            modules,
            cycle => CycleError[I, E, S](cycle.map(_.id))
          ) match {
            case Valid(filesWithContext) =>
              filesWithContext
                .foldLeft[ValidatedNec[Err, Chain[AquaProcessed[I]]]](
                  validNec(Chain.nil)
                ) {
                  case (acc, (i, Valid(context))) =>
                    acc combine validNec(Chain.one(AquaProcessed(i, context)))
                  case (acc, (_, Invalid(errs))) =>
                    acc combine Invalid(errs)
                }
                .map(
                  _.map { ap =>
                    val compiled = backend.generate(ap.context, config)
                    AquaCompiled(ap.id, compiled)
                  }
                )
            case i @ Invalid(_) => i
          }
        case i @ Invalid(_) => i
      }
  }

  def compileTo[F[_]: Monad, E, I, S[_]: Comonad, T](
    sources: AquaSources[F, E, I],
    liftI: (I, String) => LiftParser[S],
    backend: Backend,
    config: BodyConfig,
    write: AquaCompiled[I] => F[Seq[Validated[E, T]]]
  ): F[ValidatedNec[AquaError[I, E, S], Chain[T]]] =
    compile[F, E, I, S](sources, liftI, backend, config).flatMap {
      case Valid(compiled) =>
        compiled.map { ac =>
          write(ac).map(
            _.map(
              _.bimap[NonEmptyChain[AquaError[I, E, S]], Chain[T]](
                e => NonEmptyChain.one(OutputError(ac, e)),
                Chain.one
              )
            )
          )
        }.toList
          .traverse(identity)
          .map(
            _.flatten
              .foldLeft[ValidatedNec[AquaError[I, E, S], Chain[T]]](validNec(Chain.nil))(
                _ combine _
              )
          )

      case Validated.Invalid(errs) =>
        Validated.invalid[NonEmptyChain[AquaError[I, E, S]], Chain[T]](errs).pure[F]
    }
}
