package aqua.compiler

import aqua.linker.{AquaModule, Modules}
import aqua.parser.Ast
import aqua.parser.head.{FilenameExpr, ImportExpr}
import aqua.parser.lift.LiftParser
import cats.data.{Chain, NonEmptyChain, Validated, ValidatedNec}
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import cats.{Comonad, Monad}

// TODO: add tests
class AquaParser[F[_]: Monad, E, I, S[_]: Comonad](
  sources: AquaSources[F, E, I],
  liftI: (I, String) => LiftParser[S]
) {

  type Body = Ast[S]
  type Err = AquaError[I, E, S]

  // Parse all the source files
  def parseSources: F[ValidatedNec[Err, Chain[(I, Body)]]] =
    sources.sources
      .map(
        _.leftMap(_.map[Err](SourcesErr(_))).andThen(_.map { case (i, s) =>
          implicit val lift: LiftParser[S] = liftI(i, s)
          Ast.fromString[S](s).bimap(_.map[Err](ParserErr(_)), ast => Chain.one(i -> ast))
        }.foldLeft(Validated.validNec[Err, Chain[(I, Body)]](Chain.nil))(_ combine _))
      )

  // Resolve imports (not parse, just resolve) of the given file
  def resolveImports(id: I, ast: Ast[S]): F[ValidatedNec[Err, Map[I, Err]]] =
    ast.head.tailForced
      .map(_.head)
      .collect { case fe: FilenameExpr[F] =>
        sources
          .resolveImport(id, fe.fileValue)
          .map(
            _.bimap(
              _.map[Err](ResolveImportsErr(id, fe.filename, _)),
              importId => Chain.one[(I, Err)](importId -> ImportErr(fe.filename))
            )
          )
      }
      .traverse(identity)
      .map(
        _.foldLeft(Validated.validNec[Err, Chain[(I, Err)]](Chain.nil))(_ combine _)
          .map(_.toList.toMap)
      )

  // Parse sources, convert to modules
  def sourceModules: F[ValidatedNec[Err, Modules[I, Err, Body]]] =
    parseSources.flatMap {
      case Validated.Valid(srcs) =>
        srcs.traverse { case (id, ast) =>
          resolveImports(id, ast).map(_.map(AquaModule(id, _, ast)).map(Chain.one))
        }.map(
          _.foldLeft(Validated.validNec[Err, Chain[AquaModule[I, Err, Body]]](Chain.empty))(
            _ combine _
          )
        )
      case Validated.Invalid(errs) =>
        Validated.invalid[NonEmptyChain[Err], Chain[AquaModule[I, Err, Body]]](errs).pure[F]
    }.map(_.map(_.foldLeft(Modules[I, Err, Body]())(_.add(_, toExport = true))))

  def loadModule(imp: I): F[ValidatedNec[Err, AquaModule[I, Err, Ast[S]]]] =
    sources
      .load(imp)
      .map(_.leftMap(_.map[Err](SourcesErr(_))).andThen { src =>
        implicit val lift: LiftParser[S] = liftI(imp, src)
        Ast.fromString[S](src).leftMap(_.map[Err](ParserErr(_)))
      })
      .flatMap {
        case Validated.Valid(ast) =>
          resolveImports(imp, ast).map(_.map(AquaModule(imp, _, ast)))
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

  def resolve[T](transpile: Ast[S] => T => T): F[ValidatedNec[Err, Modules[I, Err, T => T]]] =
    resolveSources.map(_.map(_.map(transpile)))

}
