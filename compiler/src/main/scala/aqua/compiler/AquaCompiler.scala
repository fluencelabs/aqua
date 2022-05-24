package aqua.compiler

import aqua.backend.Backend
import aqua.linker.{AquaModule, Linker, Modules}
import aqua.model.AquaContext
import aqua.parser.lift.{LiftParser, Span}
import aqua.parser.{Ast, ParserError}
import aqua.raw.RawPart.Parts
import aqua.raw.{RawContext, RawPart}
import aqua.res.AquaRes
import aqua.semantics.{CompilerState, Semantics}
import aqua.semantics.header.HeaderSem
import cats.data.*
import cats.data.Validated.{validNec, Invalid, Valid}
import cats.parse.Parser0
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.monoid.*
import cats.syntax.traverse.*
import cats.syntax.semigroup.*
import cats.{~>, Comonad, Monad, Monoid, Order}
import scribe.Logging

trait AquaCompiler[C] extends Logging {

  type Err[I, E, S[_]] = AquaError[I, E, S]
  // TODO: find the way to replace RawContext with C; maybe move some functions to RawContext-specific subclasses, etc.
  type Ctx[I] = NonEmptyMap[I, RawContext]
  // TODO: remove CompilerState[S] from the right
  type ValidatedCtx[I, E, S[_]] = ValidatedNec[Err[I, E, S], (CompilerState[S], Ctx[I])]
  type ValidatedCtxT[I, E, S[_]] = ValidatedCtx[I, E, S] => ValidatedCtx[I, E, S]

  private def linkModules[E, I: Order, S[_]: Comonad](
    modules: Modules[
      I,
      Err[I, E, S],
      ValidatedCtxT[I, E, S]
    ],
    cycleError: List[AquaModule[I, Err[I, E, S], ValidatedCtxT[I, E, S]]] => Err[I, E, S]
  )(implicit
    rc: Monoid[RawContext]
  ): ValidatedNec[Err[I, E, S], (Chain[CompilerState[S]], Chain[AquaProcessed[I]])] = {
    logger.trace("linking modules...")

    Linker
      .link(
        modules,
        cycleError,
        // By default, provide an empty context for this module's id
        i => validNec((CompilerState[S](), NonEmptyMap.one(i, Monoid.empty[RawContext])))
      )
      .andThen { filesWithContext =>
        logger.trace("linking finished")
        filesWithContext
          .foldLeft[
            (
              ValidatedNec[Err[I, E, S], (Chain[CompilerState[S]], Chain[AquaProcessed[I]])],
              AquaContext.Cache
            )
          ](
            validNec((Chain.nil, Chain.nil)) -> AquaContext.Cache()
          ) {
            case ((acc, cache), (i, Valid(result))) =>
              val (processed, cacheProcessed) =
                result._2.toNel.toList.foldLeft[
                  ((Chain[CompilerState[S]], Chain[AquaProcessed[I]]), AquaContext.Cache)
                ](
                  (Chain.nil, Chain.nil) -> cache
                ) { case ((acc, accCache), (i, c)) =>
                  logger.trace(s"Going to prepare exports for ${i}...")
                  val (exp, expCache) = AquaContext.exportsFromRaw(c, accCache)
                  logger.trace(s"AquaProcessed prepared for ${i}")
                  (acc._1 :+ result._1, acc._2 :+ AquaProcessed(i, exp)) -> expCache
                }
              acc.combine(
                validNec(
                  processed
                )
              ) -> cacheProcessed
            case ((acc, cache), (_, Invalid(errs))) =>
              acc.combine(Invalid(errs)) -> cache
          }
          ._1

      }
  }

  private def compileRaw[F[_]: Monad, E, I: Order, S[_]: Comonad](
    sources: AquaSources[F, E, I],
    parser: I => String => ValidatedNec[ParserError[S], Ast[S]],
    config: AquaCompilerConf
  ): F[ValidatedNec[AquaError[I, E, S], (Chain[CompilerState[S]], Chain[AquaProcessed[I]])]] = {
    implicit val rc: Monoid[RawContext] = RawContext
      .implicits(
        RawContext.blank
          .copy(parts = Chain.fromSeq(config.constantsList).map(const => RawContext.blank -> const))
      )
      .rawContextMonoid
    type CErr = Err[I, E, S]
    type VCtx = ValidatedCtx[I, E, S]
    logger.trace("starting resolving sources...")
    new AquaParser[F, E, I, S](sources, parser)
      .resolve[VCtx](mod =>
        context =>
          // Context with prepared imports
          context.andThen { case (_, ctx) =>
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
                  // Handle exports, declares - finalize the resulting context
                  .andThen { case (state, ctx) =>
                    headerSem.finCtx(ctx).map(r => (state, r))
                  }
                  .map { case (state, rc) => (state, NonEmptyMap.one(mod.id, rc)) }
              }
              // The whole chain returns a semantics error finally
              .leftMap(_.map[CErr](CompileError(_)))
          }
      )
      .map(
        _.andThen { modules => linkModules(modules, cycle => CycleError[I, E, S](cycle.map(_.id))) }
      )
  }

  // Get only compiled model
  def compileToContext[F[_]: Monad, E, I: Order, S[_]: Comonad](
    sources: AquaSources[F, E, I],
    parser: I => String => ValidatedNec[ParserError[S], Ast[S]],
    config: AquaCompilerConf
  ): F[ValidatedNec[AquaError[I, E, S], (Chain[CompilerState[S]], Chain[AquaContext])]] = {
    compileRaw(sources, parser, config).map(_.map { case (st, compiled) =>
      (
        st,
        compiled.map { ap =>
          logger.trace("generating output...")
          ap.context
        }
      )
    })
  }

  // Get result generated by backend
  def compile[F[_]: Monad, E, I: Order, S[_]: Comonad](
    sources: AquaSources[F, E, I],
    parser: I => String => ValidatedNec[ParserError[S], Ast[S]],
    backend: Backend.Transform,
    config: AquaCompilerConf
  ): F[ValidatedNec[AquaError[I, E, S], Chain[AquaCompiled[I]]]] = {
    compileRaw(sources, parser, config).map(_.map { case (_, compiled) =>
      compiled.map { ap =>
        logger.trace("generating output...")
        val res = backend.transform(ap.context)
        val compiled = backend.generate(res)
        AquaCompiled(ap.id, compiled, res.funcs.length.toInt, res.services.length.toInt)
      }
    })
  }

  def compileTo[F[_]: Monad, E, I: Order, S[_]: Comonad, T](
    sources: AquaSources[F, E, I],
    parser: I => String => ValidatedNec[ParserError[S], Ast[S]],
    backend: Backend.Transform,
    config: AquaCompilerConf,
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

object AquaCompiler extends AquaCompiler[RawContext]
