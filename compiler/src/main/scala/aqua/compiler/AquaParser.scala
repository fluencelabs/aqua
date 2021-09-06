package aqua.compiler

import aqua.linker.{AquaModule, Modules}
import aqua.parser.{Ast, ParserError}
import aqua.parser.head.{FilenameExpr, ImportExpr}
import aqua.parser.lift.{LiftParser, Span}
import cats.data.{Chain, NonEmptyChain, Validated, ValidatedNec}
import cats.parse.Parser0
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import cats.{Comonad, Monad}
import scribe.Logging
import cats.~>

// TODO: add tests
class AquaParser[F[_]: Monad, E, I, S[_]: Comonad](
  sources: AquaSources[F, E, I],
  parser: I => String => ValidatedNec[ParserError[S], Ast[S]]
) extends Logging {

  type Body = Ast[S]
  type Err = AquaError[I, E, S]

  // Parse all the source files
  def parseSources: F[ValidatedNec[Err, Chain[(I, Body)]]] =
    sources.sources
      .map(
        _.leftMap(_.map[Err](SourcesErr(_))).andThen(_.map { case (i, s) =>
          parser(i)(s)
            .bimap(_.map[Err](ParserErr(_)), ast => Chain.one(i -> ast))
        }.foldLeft(Validated.validNec[Err, Chain[(I, Body)]](Chain.nil))(_ combine _))
      )

  // Resolve imports (not parse, just resolve) of the given file
  def resolveImports(id: I, ast: Body): F[ValidatedNec[Err, AquaModule[I, Err, Body]]] =
    ast.head.tailForced
      .map(_.head)
      .collect { case fe: FilenameExpr[F] =>
        sources
          .resolveImport(id, fe.fileValue)
          .map(
            _.bimap(
              _.map[Err](ResolveImportsErr(id, fe.filename, _)),
              importId =>
                Chain.one[(I, (String, Err))](importId -> (fe.fileValue, ImportErr(fe.filename)))
            )
          )
      }
      .traverse(identity)
      .map(
        _.foldLeft(Validated.validNec[Err, Chain[(I, (String, Err))]](Chain.nil))(_ combine _).map {
          collected =>
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
        }.map(
          _.foldLeft(Validated.validNec[Err, Chain[AquaModule[I, Err, Body]]](Chain.empty))(
            _ combine _
          )
        )
      case Validated.Invalid(errs) =>
        Validated.invalid[NonEmptyChain[Err], Chain[AquaModule[I, Err, Body]]](errs).pure[F]
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
          Validated.invalid[NonEmptyChain[Err], AquaModule[I, Err, Ast[S]]](errs).pure[F]
      }

  def resolveModules(
    modules: Modules[I, Err, Body]
  ): F[ValidatedNec[Err, Modules[I, Err, Ast[S]]]] =
    modules.dependsOn.map { case (moduleId, unresolvedErrors) =>
      loadModule(moduleId).map(_.leftMap(_ ++ unresolvedErrors))
    }.toList
      .traverse(identity)
      .map(_.foldLeft[ValidatedNec[Err, Modules[I, Err, Ast[S]]]](Validated.validNec(modules)) {
        case (mods, m) =>
          mods.andThen(ms => m.map(ms.add(_)))
      })
      .flatMap {
        case Validated.Valid(ms) if ms.isResolved =>
          Validated.validNec[Err, Modules[I, Err, Ast[S]]](ms).pure[F]
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
