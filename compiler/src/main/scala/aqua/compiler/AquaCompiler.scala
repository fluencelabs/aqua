package aqua.compiler

import aqua.compiler.AquaError.{ParserError as AquaParserError, *}
import aqua.linker.{AquaModule, Linker, Modules}
import aqua.parser.{Ast, ParserError}
import aqua.semantics.header.{HeaderHandler, Picker}
import aqua.semantics.{SemanticError, Semantics}

import cats.arrow.FunctionK
import cats.data.*
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import cats.{Comonad, Monad, Monoid, Order, ~>}
import scribe.Logging

class AquaCompiler[F[_]: Monad, E, I: Order, S[_]: Comonad, C: Monoid: Picker](
  headerHandler: HeaderHandler[S, C],
  semantics: Semantics[S, C]
) extends Logging {

  type Err = AquaError[I, E, S]
  type Ctx = NonEmptyMap[I, C]

  type CompileWarns = [A] =>> CompileWarnings[S][A]
  type CompileRes = [A] =>> CompileResult[I, E, S][A]

  type CompiledCtx = CompileRes[Ctx]
  type CompiledCtxT = CompiledCtx => CompiledCtx

  private def linkModules(
    modules: Modules[I, Err, CompiledCtxT],
    cycleError: Linker.DepCycle[AquaModule[I, Err, CompiledCtxT]] => Err
  ): CompileRes[Map[I, C]] = {
    logger.trace("linking modules...")

    // By default, provide an empty context for this module's id
    val empty: I => CompiledCtx = i => NonEmptyMap.one(i, Monoid[C].empty).pure[CompileRes]

    for {
      linked <- Linker
        .link(modules, cycleError, empty)
        .toEither
        .toEitherT[CompileWarns]
      res <- EitherT(
        linked.toList.traverse { case (id, ctx) =>
          ctx
            .map(
              /**
               * NOTE: This should be safe
               * as result for id should contain itself
               */
              _.apply(id).map(id -> _).get
            )
            .toValidated
        }.map(_.sequence.toEither)
      )
    } yield res.toMap
  }

  def compileRaw(
    sources: AquaSources[F, E, I],
    parser: I => String => ValidatedNec[ParserError[S], Ast[S]]
  ): F[CompileRes[Map[I, C]]] = {
    logger.trace("starting resolving sources...")

    new AquaParser[F, E, I, S](sources, parser)
      .resolve[CompiledCtx](mod =>
        context =>
          for {
            // Context with prepared imports
            ctx <- context
            imports = mod.imports.flatMap { case (fn, id) =>
              ctx.apply(id).map(fn -> _)
            }
            header = mod.body.head
            headerSem <- headerHandler
              .sem(imports, header)
              .toCompileRes
            // Analyze the body, with prepared initial context
            _ = logger.trace("semantic processing...")
            processed <- semantics
              .process(
                mod.body,
                headerSem.initCtx
              )
              .toCompileRes
            // Handle exports, declares - finalize the resulting context
            rc <- headerSem
              .finCtx(processed)
              .toCompileRes
            /**
             * Here we build a map of contexts while processing modules.
             * Should not linker provide this info inside this process?
             * Building this map complicates things a lot.
             */
          } yield NonEmptyMap.one(mod.id, rc)
      )
      .value
      .map(resolved =>
        for {
          modules <- resolved.toEitherT[CompileWarns]
          linked <- linkModules(
            modules,
            cycle => CycleError(cycle.map(_.id))
          )
        } yield linked
      )
  }

  private val warningsK: semantics.Warnings ~> CompileWarns =
    new FunctionK[semantics.Warnings, CompileWarns] {

      override def apply[A](
        fa: semantics.Warnings[A]
      ): CompileWarns[A] =
        fa.mapWritten(_.map(AquaWarning.CompileWarning.apply))
    }

  extension (res: semantics.ProcessResult) {

    def toCompileRes: CompileRes[C] =
      res
        .leftMap(_.map(CompileError.apply))
        .mapK(warningsK)
  }

  extension [A](res: ValidatedNec[SemanticError[S], A]) {

    def toCompileRes: CompileRes[A] =
      res.toEither
        .leftMap(_.map(CompileError.apply))
        .toEitherT[CompileWarns]
  }

}
