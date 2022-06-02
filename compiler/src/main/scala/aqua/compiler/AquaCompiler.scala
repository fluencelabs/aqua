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
import aqua.semantics.header.{HeaderHandler, HeaderSem, Picker}
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

class AquaCompiler[F[_]: Monad, E, I: Order, S[_]: Comonad, C: Monoid: Picker](
  headerHandler: HeaderHandler[S, C],
  semantics: Semantics[S, C]
) extends Logging {

  type Err = AquaError[I, E, S]
  type Ctx = NonEmptyMap[I, C]

  type ValidatedCtx = ValidatedNec[Err, Ctx]
  type ValidatedCtxT = ValidatedCtx => ValidatedCtx

  private def linkModules(
    modules: Modules[
      I,
      Err,
      ValidatedCtxT
    ],
    cycleError: List[AquaModule[I, Err, ValidatedCtxT]] => Err
  ): ValidatedNec[Err, Map[I, ValidatedCtx]] = {
    logger.trace("linking modules...")

    Linker
      .link(
        modules,
        cycleError,
        // By default, provide an empty context for this module's id
        i => validNec(NonEmptyMap.one(i, Monoid.empty[C]))
      )
  }

  def compileRaw(
    sources: AquaSources[F, E, I],
    parser: I => String => ValidatedNec[ParserError[S], Ast[S]]
  ): F[Validated[NonEmptyChain[Err], Map[I, ValidatedCtx]]] = {

    logger.trace("starting resolving sources...")
    new AquaParser[F, E, I, S](sources, parser)
      .resolve[ValidatedCtx](mod =>
        context =>
          // Context with prepared imports
          context.andThen { ctx =>
            // To manage imports, exports run HeaderHandler
            headerHandler
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
                semantics
                  .process(
                    mod.body,
                    headerSem.initCtx
                  )
                  // Handle exports, declares - finalize the resulting context
                  .andThen { ctx =>
                    headerSem.finCtx(ctx)
                  }
                  .map { rc => NonEmptyMap.one(mod.id, rc) }
              }
              // The whole chain returns a semantics error finally
              .leftMap(_.map[Err](CompileError(_)))
          }
      )
      .map(
        _.andThen { modules => linkModules(modules, cycle => CycleError[I, E, S](cycle.map(_.id))) }
      )
  }

}
