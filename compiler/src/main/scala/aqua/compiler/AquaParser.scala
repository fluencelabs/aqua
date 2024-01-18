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

  private def parse(id: I, src: String): ValidatedNec[Err, (I, Body)] =
    parser(id)(src).bimap(
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
        .traverse(parse.tupled)
        .toEither
        .toEitherT
    } yield parsed

  private def loadModule(id: I): F[ValidatedNec[Err, AquaModule[I, Err, Body]]] =
    (for {
      src <- EitherT
        .fromValidatedF(sources.load(id))
        .leftMap(_.map(SourcesError.apply))
      parsed <- parse(id, src).toEither.toEitherT
      (id, ast) = parsed
      resolved <- EitherT.fromValidatedF(
        resolveImports(id, ast)
      )
    } yield resolved).toValidated

  // Resolve imports (not parse, just resolve) of the given file
  private def resolveImports(id: I, ast: Body): F[ValidatedNec[Err, AquaModule[I, Err, Body]]] =
    ast.head.collect { case fe: FilenameExpr[S] =>
      fe.fileValue -> fe.token
    }.traverse { case (filename, token) =>
      sources
        .resolveImport(id, filename)
        .map(
          _.bimap(
            _.map(ResolveImportsError(id, token, _): Err),
            importId => importId -> (filename, ImportError(token): Err)
          )
        )
    }.map(_.sequence.map { collected =>
      AquaModule(
        id = id,
        // How filenames correspond to the resolved IDs
        imports = collected.map { case (i, (fn, _)) =>
          fn -> i
        }.toList.toMap[String, I],
        // Resolved IDs to errors that point to the import in source code
        dependsOn = collected.map { case (i, (_, err)) =>
          i -> err
        }.toList.toMap[I, Err],
        body = ast
      )
    })

  // Parse sources, convert to modules
  private def sourceModules: FE[Modules[I, Err, Body]] =
    for {
      srcs <- parseSources
      modules <- EitherT.fromValidatedF(
        srcs.traverse(resolveImports.tupled).map(_.sequence)
      )
    } yield Modules.from(modules)

  private def resolveModules(
    modules: Modules[I, Err, Body]
  ): FE[Modules[I, Err, Ast[S]]] =
    modules.iterateUntilM(ms =>
      EitherT
        .fromValidatedF(
          // Load all modules that are dependencies of the current modules
          ms.dependsOn.toList.traverse { case (moduleId, unresolvedErrors) =>
            loadModule(moduleId).map(
              _.leftMap(_ ++ unresolvedErrors)
            )
          }.map(_.sequence)
        )
        // Add all loaded modules to the current modules
        .map(ms.addAll)
    )(_.isResolved)

  def resolve[T](
    transpile: AquaModule[I, Err, Body] => T => T
  ): FE[Modules[I, Err, T => T]] =
    (sourceModules >>= resolveModules)
      .map(_.mapModuleToBody(transpile))

}
