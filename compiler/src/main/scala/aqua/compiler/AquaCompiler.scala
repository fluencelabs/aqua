package aqua.compiler

import aqua.compiler.AquaError.{ParserError as AquaParserError, *}
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
import aqua.semantics.{SemanticError, SemanticWarning}

import cats.data.*
import cats.data.Validated.{validNec, Invalid, Valid}
import cats.parse.Parser0
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.monoid.*
import cats.syntax.traverse.*
import cats.syntax.semigroup.*
import cats.syntax.either.*
import cats.{~>, Comonad, Functor, Monad, Monoid, Order}
import cats.arrow.FunctionK
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
          ctx.map(_.apply(id).map(id -> _).get).toValidated
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

  private val warningsK: semantics.ProcessWarnings ~> CompileWarns =
    new FunctionK[semantics.ProcessWarnings, CompileWarns] {

      override def apply[A](
        fa: semantics.ProcessWarnings[A]
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
