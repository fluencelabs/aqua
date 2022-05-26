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
import aqua.semantics.header.{HeaderSem, HeaderSemAct}
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

class AquaCompiler[C](implicit
  rc: Monoid[C]
) extends Logging {

  type Err[I, E, S[_]] = AquaError[I, E, S]
  type Ctx[I] = NonEmptyMap[I, C]
  // TODO: remove CompilerState[S] from the left
  type ValidatedCtx[I, E, S[_]] = ValidatedNec[Err[I, E, S], (CompilerState[S], Ctx[I])]
  type ValidatedCtxT[I, E, S[_]] = ValidatedCtx[I, E, S] => ValidatedCtx[I, E, S]

  private def linkModules[E, I: Order, S[_]: Comonad](
    modules: Modules[
      I,
      Err[I, E, S],
      ValidatedCtxT[I, E, S]
    ],
    cycleError: List[AquaModule[I, Err[I, E, S], ValidatedCtxT[I, E, S]]] => Err[I, E, S]
  ): ValidatedNec[Err[I, E, S], Map[I, ValidatedCtx[I, E, S]]] = {
    logger.trace("linking modules...")

    Linker
      .link(
        modules,
        cycleError,
        // By default, provide an empty context for this module's id
        i => validNec((CompilerState[S](), NonEmptyMap.one(i, Monoid.empty[C])))
      )
  }

  def compileRaw[F[_]: Monad, E, I: Order, S[_]: Comonad](
    sources: AquaSources[F, E, I],
    parser: I => String => ValidatedNec[ParserError[S], Ast[S]],
    header: HeaderSemAct[S, C]
  ): F[Validated[NonEmptyChain[Err[I, E, S]], Map[I, ValidatedCtx[I, E, S]]]] = {

    type CErr = Err[I, E, S]
    type VCtx = ValidatedCtx[I, E, S]
    logger.trace("starting resolving sources...")
    new AquaParser[F, E, I, S](sources, parser)
      .resolve[VCtx](mod =>
        context =>
          // Context with prepared imports
          context.andThen { case (_, ctx) =>
            // To manage imports, exports run HeaderSem
            header
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

}
