package aqua.compiler

import aqua.linker.{AquaModule, Modules}
import aqua.parser.head.{FilenameExpr, ImportExpr}
import aqua.parser.lift.{LiftParser, Span}
import aqua.parser.{Ast, ParserError}
import cats.data.{Chain, NonEmptyChain, Validated, ValidatedNec}
import cats.parse.Parser0
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.monad.*
import cats.syntax.foldable.*
import cats.syntax.validated.*
import cats.data.Chain.*
import cats.data.Validated.*
import cats.syntax.traverse.*
import cats.{~>, Comonad, Monad}
import scribe.Logging

// TODO: add tests
class AquaParser[F[_], E, I, S[_]: Comonad](
  sources: AquaSources[F, E, I],
  parser: I => String => ValidatedNec[ParserError[S], Ast[S]]
)(implicit F: Monad[F])
    extends Logging {

  type Body = Ast[S]
  type Err = AquaError[I, E, S]

  // Parse all the source files
  def parseSources: F[ValidatedNec[Err, Chain[(I, Body)]]] =
    sources.sources.map(
      _.leftMap(_.map[Err](SourcesErr(_))).andThen(_.map { case (i, s) =>
        parser(i)(s)
          .bimap(
            _.map[Err](ParserErr(_)),
            ast => Chain.one(i -> ast)
          )
      }.foldA)
    )

  // Resolve imports (not parse, just resolve) of the given file
  def resolveImports(id: I, ast: Body): F[ValidatedNec[Err, AquaModule[I, Err, Body]]] =
    ast.head.tailForced
      .map(_.head)
      .collect { case fe: FilenameExpr[F] =>
        F.map(
          sources
            .resolveImport(id, fe.fileValue)
        )(
          _.bimap(
            _.map[Err](ResolveImportsErr(id, fe.filename, _)),
            importId =>
              Chain.one[(I, (String, Err))](importId -> (fe.fileValue, ImportErr(fe.filename)))
          )
        )
      }
      .sequence
      .map(
        _.foldA.map { collected =>
          AquaModule[I, Err, Body](
            id,
            // How filenames correspond to the resolved IDs
            collected.map { case (i, (fn, _)) =>
              fn -> i
            }.toList.toMap[String, I],
            // Resolved IDs to errors that point to the import in source code
            collected.map { case (i, (_, err)) =>
              i -> err
            }.toList.toMap[I, Err],
            ast
          )
        }
      )

  // Parse sources, convert to modules
  def sourceModules: F[ValidatedNec[Err, Modules[I, Err, Body]]] =
    parseSources.flatMap {
      case Validated.Valid(srcs) =>
        srcs.traverse { case (id, ast) =>
          resolveImports(id, ast).map(_.map(Chain.one))
        }.map(_.foldA)
      case Validated.Invalid(errs) =>
        errs.invalid.pure[F]
    }.map(_.map(_.foldLeft(Modules[I, Err, Body]())(_.add(_, toExport = true))))

  def loadModule(imp: I): F[ValidatedNec[Err, AquaModule[I, Err, Body]]] =
    sources
      .load(imp)
      .map(_.leftMap(_.map[Err](SourcesErr(_))).andThen { src =>
        parser(imp)(src).leftMap(_.map[Err](ParserErr(_)))
      })
      .flatMap {
        case Validated.Valid(ast) =>
          resolveImports(imp, ast)
        case Validated.Invalid(errs) =>
          errs.invalid.pure[F]
      }

  def resolveModules(
    modules: Modules[I, Err, Body]
  ): F[ValidatedNec[Err, Modules[I, Err, Ast[S]]]] =
    modules.dependsOn.map { case (moduleId, unresolvedErrors) =>
      loadModule(moduleId).map(_.leftMap(_ ++ unresolvedErrors))
    }.toList.sequence
      .map(
        _.foldLeft(modules.validNec[Err]) { case (mods, m) =>
          mods.andThen(ms => m.map(ms.add(_)))
        }
      )
      .flatMap {
        case Validated.Valid(ms) if ms.isResolved =>
          ms.validNec.pure[F]
        case Validated.Valid(ms) =>
          resolveModules(ms)
        case err =>
          err.pure[F]
      }

  def resolveSources: F[ValidatedNec[Err, Modules[I, Err, Ast[S]]]] =
    sourceModules.flatMap {
      case Validated.Valid(ms) => resolveModules(ms)
      case err => err.pure[F]
    }

  def resolve[T](
    transpile: AquaModule[I, Err, Body] => T => T
  ): F[ValidatedNec[Err, Modules[I, Err, T => T]]] =
    resolveSources.map(_.map(_.mapModuleToBody(transpile)))

}
