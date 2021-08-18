package aqua.compiler

import aqua.backend.Backend
import aqua.linker.Linker
import aqua.model.AquaContext
import aqua.model.transform.TransformConfig
import aqua.model.transform.res.AquaRes
import aqua.parser.lift.LiftParser
import aqua.semantics.Semantics
import cats.data.Validated.{validNec, Invalid, Valid}
import cats.data.{Chain, NonEmptyChain, Validated, ValidatedNec}
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import cats.{Comonad, Monad, Monoid}
import scribe.Logging

object AquaCompiler extends Logging {

  def compile[F[_]: Monad, E, I, S[_]: Comonad](
    sources: AquaSources[F, E, I],
    liftI: (I, String) => LiftParser[S],
    backend: Backend,
    config: TransformConfig
  ): F[ValidatedNec[AquaError[I, E, S], Chain[AquaCompiled[I]]]] = {
    import config.aquaContextMonoid
    type Err = AquaError[I, E, S]
    type Ctx = Map[I, AquaContext]
    type ValidatedCtx = ValidatedNec[Err, Ctx]

    new AquaParser[F, E, I, S](sources, liftI)
      .resolve[ValidatedCtx](ast =>
        context =>
          context.andThen(ctx =>
            // TODO: for the ast, we should know how it was derived
            Semantics
              .process(
                ast,
                // TODO: there should be exactly one value
                ctx.values
                  .reduceLeftOption(Monoid[AquaContext].combine)
                  .getOrElse(Monoid[AquaContext].empty)
              )
              .leftMap(_.map[Err](CompileError(_)))
          )
      )
      .map(
        _.andThen(modules =>
          Linker
            .link[I, AquaError[I, E, S], ValidatedCtx](
              modules,
              cycle => CycleError[I, E, S](cycle.map(_.id))
            )
            .andThen { filesWithContext =>
              filesWithContext
                .foldLeft[ValidatedNec[Err, Chain[AquaProcessed[I]]]](
                  validNec(Chain.nil)
                ) {
                  case (acc, (i, Valid(context))) =>
                    acc combine validNec(
                      Chain.fromSeq(context.toSeq.map((i, c) => AquaProcessed(i, c)))
                    )
                  case (acc, (_, Invalid(errs))) =>
                    acc combine Invalid(errs)
                }
                .map(
                  _.map { ap =>
                    val compiled = backend.generate(AquaRes.fromContext(ap.context, config))
                    AquaCompiled(ap.id, compiled)
                  }
                )
            }
        )
      )
  }

  def compileTo[F[_]: Monad, E, I, S[_]: Comonad, T](
    sources: AquaSources[F, E, I],
    liftI: (I, String) => LiftParser[S],
    backend: Backend,
    config: TransformConfig,
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
