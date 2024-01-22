package aqua.compiler

import aqua.compiler.AquaError.{ParserError as AquaParserError, *}
import aqua.linker.{AquaModule, Modules}
import aqua.parser.head.{FilenameExpr, ImportExpr}
import aqua.parser.lift.{LiftParser, Span}
import aqua.parser.{Ast, ParserError}
import aqua.syntax.eithert.fromValidatedF

import cats.data.Chain.*
import cats.data.Validated.*
import cats.data.{Chain, EitherNec, EitherT, NonEmptyChain, Validated, ValidatedNec}
import cats.parse.Parser0
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.monad.*
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import cats.syntax.validated.*
import cats.{Comonad, Monad, ~>}
import scribe.Logging

// TODO: add tests
class AquaParser[F[_]: Monad, E, I, S[_]: Comonad](
  sources: AquaSources[F, E, I],
  parser: I => String => ValidatedNec[ParserError[S], Ast[S]]
) extends Logging {

  type Body = Ast[S]
  type Err = AquaError[I, E, S]

  private type FE[A] = EitherT[F, NonEmptyChain[Err], A]

  // Parse one source (text)
  private def parse(id: I, src: String): EitherNec[Err, (I, Body)] =
    parser(id)(src).toEither.bimap(
      _.map(AquaParserError.apply),
      ast => id -> ast
    )

  // Parse all the source files
  private def parseSources: FE[Chain[(I, Body)]] =
    for {
      srcs <- EitherT
        .fromValidatedF(sources.sources)
        .leftMap(_.map(SourcesError.apply))
      parsed <- srcs
        .parTraverse(parse.tupled)
        .toEitherT
    } yield parsed

  // Load one module (parse, resolve imports)
  private def loadModule(id: I): FE[AquaModule[I, Err, Body]] =
    for {
      src <- EitherT
        .fromValidatedF(sources.load(id))
        .leftMap(_.map(SourcesError.apply))
      parsed <- parse(id, src).toEitherT
      (id, ast) = parsed
      resolved <- resolveImports(id, ast)
    } yield resolved

  // Resolve imports (not parse, just resolve) of the given file
  private def resolveImports(id: I, ast: Body): FE[AquaModule[I, Err, Body]] =
    ast.head.collect { case fe: FilenameExpr[S] =>
      fe.fileValue -> fe.token
    }.parTraverse { case (filename, token) =>
      EitherT
        .fromValidatedF(
          sources.resolveImport(id, filename)
        )
        .bimap(
          _.map(ResolveImportsError(id, token, _): Err),
          importId => importId -> (filename, ImportError(token): Err)
        )
    }.map { collected =>
      AquaModule(
        id = id,
        // How filenames correspond to the resolved IDs
        imports = collected.map { case (i, (fn, _)) =>
          fn -> i
        }.toList.toMap,
        // Resolved IDs to errors that point to the import in source code
        dependsOn = collected.map { case (i, (_, err)) =>
          i -> err
        }.toList.toMap,
        body = ast
      )
    }

  // Load modules (parse, resolve imports) of all the source files
  private lazy val loadModules: FE[Modules[I, Err, Body]] =
    for {
      srcs <- parseSources
      modules <- srcs.parTraverse(resolveImports.tupled)
    } yield Modules.from(modules)

  // Resolve modules (load all the dependencies)
  private def resolveModules(
    modules: Modules[I, Err, Body]
  ): FE[Modules[I, Err, Ast[S]]] =
    modules.iterateUntilM(ms =>
      // Load all modules that are dependencies of the current modules
      ms.dependsOn.toList.parTraverse { case (moduleId, unresolvedErrors) =>
        loadModule(moduleId).leftMap(_ ++ unresolvedErrors)
      }.map(ms.addAll) // Add all loaded modules to the current modules
    )(_.isResolved)

  lazy val resolve: FE[Modules[I, Err, Body]] =
    loadModules >>= resolveModules

}
