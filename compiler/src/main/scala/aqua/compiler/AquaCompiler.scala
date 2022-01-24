package aqua.compiler

import aqua.backend.Backend
import aqua.linker.Linker
import aqua.model.AquaContext
import aqua.model.transform.TransformConfig
import aqua.model.transform.Transform
import aqua.parser.lift.{LiftParser, Span}
import aqua.parser.{Ast, ParserError}
import aqua.raw.RawContext
import aqua.res.AquaRes
import aqua.semantics.Semantics
import aqua.semantics.header.HeaderSem
import cats.data.*
import cats.data.Validated.{validNec, Invalid, Valid}
import cats.parse.Parser0
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.monoid.*
import cats.syntax.traverse.*
import cats.{~>, Comonad, Monad, Monoid, Order}
import scribe.Logging

object AquaCompiler extends Logging {

  private def compileRaw[F[_]: Monad, E, I: Order, S[_]: Comonad](
    sources: AquaSources[F, E, I],
    parser: I => String => ValidatedNec[ParserError[S], Ast[S]],
    config: TransformConfig
  ): F[ValidatedNec[AquaError[I, E, S], Chain[AquaProcessed[I]]]] = {
    implicit val rc = RawContext.implicits(RawContext.blank).rawContextMonoid
    type Err = AquaError[I, E, S]
    type Ctx = NonEmptyMap[I, RawContext]
    type ValidatedCtx = ValidatedNec[Err, Ctx]
    logger.trace("starting resolving sources...")
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
                logger.trace("semantic processing...")
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
        _.andThen { modules =>
          logger.trace("linking modules...")
          Linker
            .link[I, AquaError[I, E, S], ValidatedCtx](
              modules,
              cycle => CycleError[I, E, S](cycle.map(_.id)),
              // By default, provide an empty context for this module's id
              i => validNec(NonEmptyMap.one(i, Monoid.empty[RawContext]))
            )
            .andThen { filesWithContext =>
              logger.trace("linking finished")
              filesWithContext
                .foldLeft[ValidatedNec[Err, Chain[AquaProcessed[I]]]](
                  validNec(Chain.nil)
                ) {
                  case (acc, (i, Valid(context))) =>
                    acc combine validNec(
                      Chain.fromSeq(context.toNel.toList.map { case (i, c) =>
                        logger.trace(s"Processed ${i}")
                        AquaProcessed(i, AquaContext.fromRaw(c))
                      })
                    )
                  case (acc, (_, Invalid(errs))) =>
                    acc combine Invalid(errs)
                }
            }
        }
      )
  }

  // Get only compiled model
  def compileToContext[F[_]: Monad, E, I: Order, S[_]: Comonad](
    sources: AquaSources[F, E, I],
    parser: I => String => ValidatedNec[ParserError[S], Ast[S]],
    config: TransformConfig
  ): F[ValidatedNec[AquaError[I, E, S], Chain[AquaContext]]] = {
    compileRaw(sources, parser, config).map(_.map {
      _.map { ap =>
        logger.trace("generating output...")
        ap.context
      }
    })
  }

  // Get result generated by backend
  def compile[F[_]: Monad, E, I: Order, S[_]: Comonad](
    sources: AquaSources[F, E, I],
    parser: I => String => ValidatedNec[ParserError[S], Ast[S]],
    backend: Backend,
    config: TransformConfig
  ): F[ValidatedNec[AquaError[I, E, S], Chain[AquaCompiled[I]]]] = {
    compileRaw(sources, parser, config).map(_.map {
      _.map { ap =>
        logger.trace("generating output...")
        val res = Transform.contextRes(ap.context, config)
        val compiled = backend.generate(res)
        AquaCompiled(ap.id, compiled, res.funcs.length.toInt, res.services.length.toInt)
      }
    })
  }

  def compileTo[F[_]: Monad, E, I: Order, S[_]: Comonad, T](
    sources: AquaSources[F, E, I],
    parser: I => String => ValidatedNec[ParserError[S], Ast[S]],
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
