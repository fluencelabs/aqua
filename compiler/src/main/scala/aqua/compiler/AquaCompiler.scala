package aqua.compiler

import aqua.backend.Backend
import aqua.linker.Linker
import aqua.model.AquaContext
import aqua.model.transform.TransformConfig
import aqua.model.transform.res.AquaRes
import aqua.parser.lift.{LiftParser, Span}
import aqua.parser.{Ast, ParserError}
import aqua.semantics.Semantics
import aqua.semantics.header.HeaderSem
import cats.data.Validated.{validNec, Invalid, Valid}
import cats.data.{Chain, NonEmptyChain, NonEmptyMap, Validated, ValidatedNec}
import cats.parse.Parser0
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import cats.syntax.monoid.*
import cats.{Comonad, Monad, Monoid, Order}
import scribe.Logging
import cats.~>

object AquaCompiler extends Logging {

  def compile[F[_]: Monad, E, I: Order, S[_]: Comonad](
    sources: AquaSources[F, E, I],
    parser: (I, String) => ValidatedNec[ParserError[S], Ast[S]],
    backend: Backend,
    config: TransformConfig
  ): F[ValidatedNec[AquaError[I, E, S], Chain[AquaCompiled[I]]]] = {
    import config.aquaContextMonoid
    type Err = AquaError[I, E, S]
    type Ctx = NonEmptyMap[I, AquaContext]
    type ValidatedCtx = ValidatedNec[Err, Ctx]

    new AquaParser[F, E, I, S](sources, parser)
      .resolve[ValidatedCtx](mod =>
        context =>
          // Context with prepared imports
          context.andThen(ctx =>
            // To manage imports, exports run HeaderSem
            HeaderSem
              .sem(
                mod.imports.view
                  .mapValues(ctx(_))
                  .collect { case (fn, Some(fc)) => fn -> fc }
                  .toMap,
                mod.body.head
              )
              .andThen { headerSem =>
                // Analyze the body, with prepared initial context
                Semantics
                  .process(
                    mod.body,
                    headerSem.initCtx
                  )
                  // Handle exports, declares – finalize the resulting context
                  .andThen(headerSem.finCtx)
                  .map(rc => NonEmptyMap.one(mod.id, rc))
              }
              // The whole chain returns a semantics error finally
              .leftMap(_.map[Err](CompileError(_)))
          )
      )
      .map(
        _.andThen(modules =>
          Linker
            .link[I, AquaError[I, E, S], ValidatedCtx](
              modules,
              cycle => CycleError[I, E, S](cycle.map(_.id)),
              // By default, provide an empty context for this module's id
              i => validNec(NonEmptyMap.one(i, Monoid.empty[AquaContext]))
            )
            .andThen { filesWithContext =>
              filesWithContext
                .foldLeft[ValidatedNec[Err, Chain[AquaProcessed[I]]]](
                  validNec(Chain.nil)
                ) {
                  case (acc, (i, Valid(context))) =>
                    acc combine validNec(
                      Chain.fromSeq(context.toNel.toList.map { case (i, c) => AquaProcessed(i, c) })
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

  def compileTo[F[_]: Monad, E, I: Order, S[_]: Comonad, T](
    sources: AquaSources[F, E, I],
    parser: (I, String) => ValidatedNec[ParserError[S], Ast[S]],
    backend: Backend,
    config: TransformConfig,
    write: AquaCompiled[I] => F[Seq[Validated[E, T]]]
  ): F[ValidatedNec[AquaError[I, E, S], Chain[T]]] =
    compile[F, E, I, S](sources, parser, backend, config).flatMap {
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
