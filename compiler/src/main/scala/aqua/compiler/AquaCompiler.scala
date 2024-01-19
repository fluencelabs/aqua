package aqua.compiler

import aqua.backend.Backend
import aqua.compiler.AquaError.{ParserError as AquaParserError, *}
import aqua.linker.{AquaModule, Linker, Modules}
import aqua.model.AquaContext
import aqua.parser.lift.{LiftParser, Span}
import aqua.parser.{Ast, ParserError}
import aqua.raw.RawPart.Parts
import aqua.raw.{RawContext, RawPart}
import aqua.res.AquaRes
import aqua.semantics.header.{HeaderHandler, HeaderSem, Picker}
import aqua.semantics.{CompilerState, Semantics}
import aqua.semantics.{SemanticError, SemanticWarning}

import cats.arrow.FunctionK
import cats.data.*
import cats.data.Validated.{Invalid, Valid, validNec}
import cats.effect.Sync
import cats.effect.syntax.clock.*
import cats.parse.Parser0
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.monoid.*
import cats.syntax.semigroup.*
import cats.syntax.traverse.*
import cats.{Comonad, Functor, Monad, Monoid, Order, ~>}
import scribe.Logging

class AquaCompiler[F[_]: Sync, E, I: Order, S[_]: Comonad, C: Monoid: Picker](
  headerHandler: HeaderHandler[S, C],
  semantics: Semantics[S, C]
) extends Logging {

  type Err = AquaError[I, E, S]

  type CompileWarns = [A] =>> CompileWarnings[S][A]
  type CompileRes = [A] =>> CompileResult[I, E, S][A]

  // Transpilation - (Imports => Compilation Result)
  type TP = Map[String, C] => CompileRes[C]

  private def transpile(body: Ast[S]): TP =
    imports =>
      for {
        headerSem <- headerHandler
          .sem(imports, body.head)
          .toCompileRes
        // Analyze the body, with prepared initial context
        _ = logger.trace("semantic processing...")
        processed <- semantics
          .process(body, headerSem.initCtx)
          .toCompileRes
        // Handle exports, declares - finalize the resulting context
        rc <- headerSem
          .finCtx(processed)
          .toCompileRes
      } yield rc

  def compileRaw(
    sources: AquaSources[F, E, I],
    parser: I => String => ValidatedNec[ParserError[S], Ast[S]]
  ): F[CompileRes[Map[I, C]]] = {
    logger.trace("starting resolving sources...")

    val modules = new AquaParser[F, E, I, S](sources, parser).resolve

    modules
      .map(_.map(body => transpile(body)))
      .timed
      .semiflatMap { case (time, res) =>
        Sync[F].delay {
          println(s"Resolution took ${time.toMillis} ms")
          res
        }
      }
      .value
      .flatMap(resolved =>
        Sync[F].delay {
          for {
            modules <- resolved.toEitherT[CompileWarns]
            linked <- Linker.link(modules, CycleError.apply)
          } yield linked
        }.timed.flatMap((time, res) =>
          Sync[F].delay {
            println(s"Linking took ${time.toMillis} ms")
            res
          }
        )
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
