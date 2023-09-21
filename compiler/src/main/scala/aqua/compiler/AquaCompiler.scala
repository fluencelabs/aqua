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
  type Warn = AquaWarning[S]
  type Ctx = NonEmptyMap[I, C]

  type CompileWarnings =
    [A] =>> Writer[Chain[Warn], A]

  type CompileResult =
    [A] =>> EitherT[CompileWarnings, NonEmptyChain[Err], A]

  private val warningsK: semantics.ProcessWarnings ~> CompileWarnings =
    new FunctionK[semantics.ProcessWarnings, CompileWarnings] {

      override def apply[A](
        fa: semantics.ProcessWarnings[A]
      ): CompileWarnings[A] =
        fa.mapWritten(_.map(AquaWarning.CompileWarning.apply))
    }

  extension (res: semantics.ProcessResult) {

    def toCompileResult: CompileResult[C] =
      res
        .leftMap(_.map(CompileError.apply))
        .mapK(warningsK)
  }

  extension [A](res: ValidatedNec[SemanticError[S], A]) {

    def toCompileResult: CompileResult[A] =
      res.toEither
        .leftMap(_.map(CompileError.apply))
        .toEitherT[CompileWarnings]
  }

  type CompiledCtx = CompileResult[Ctx]
  type CompiledCtxT = CompiledCtx => CompiledCtx

  private def linkModules(
    modules: Modules[I, Err, CompiledCtxT],
    cycleError: Linker.DepCycle[AquaModule[I, Err, CompiledCtxT]] => Err
  ): EitherNec[Err, Map[I, CompiledCtx]] = {
    logger.trace("linking modules...")

    Linker
      .link(
        modules,
        cycleError,
        // By default, provide an empty context for this module's id
        i => NonEmptyMap.one(i, Monoid.empty[C]).asRight.toEitherT
      )
      .toEither
  }

  def compileRaw(
    sources: AquaSources[F, E, I],
    parser: I => String => ValidatedNec[ParserError[S], Ast[S]]
  ): F[ValidatedNec[Err, Map[I, ValidatedNec[Err, Ctx]]]] = {

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
              .toCompileResult
            // Analyze the body, with prepared initial context
            _ = logger.trace("semantic processing...")
            processed <- semantics
              .process(
                mod.body,
                headerSem.initCtx
              )
              .toCompileResult
            // Handle exports, declares - finalize the resulting context
            rc <- headerSem
              .finCtx(processed)
              .toCompileResult
          } yield NonEmptyMap.one(mod.id, rc)
      )
      .subflatMap(modules =>
        linkModules(
          modules,
          cycle => CycleError[I, E, S](cycle.map(_.id))
        )
      )
      .map(_.mapValues(_.toValidated.value).toMap)
      .toValidated
  }

}
