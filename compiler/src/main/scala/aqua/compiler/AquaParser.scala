package aqua.compiler

import aqua.linker.{AquaModule, Modules}
import aqua.parser.head.ImportExpr
import aqua.parser.lexer.Token
import aqua.parser.lift.LiftParser
import aqua.parser.{Ast, ParserError}
import cats.{Comonad, Monad}
import cats.data.{Chain, NonEmptyChain, Validated, ValidatedNec}
import cats.syntax.traverse._
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.applicative._

class AquaParser[F[_]: Monad, E, I, S[_]: Comonad, T](
  sources: AquaSources[F, E, I],
  liftI: I => LiftParser[S],
  transpile: Ast[S] => T => T
) {

  trait Err
  case class SourcesErr(err: E) extends Err
  case class ParserErr(err: ParserError[S]) extends Err
  case class ResolveImportsErr(fromFile: I, token: Token[S], err: E) extends Err

  def parseSources: F[ValidatedNec[Err, Chain[(I, Ast[S])]]] =
    sources.sources
      .map(
        _.andThen(_.map { case (i, s) =>
          implicit val lift: LiftParser[S] = liftI(i)
          Ast.fromString[S](s).bimap(_.map[Err](ParserErr), ast => Chain.one(i -> ast))
        }.foldLeft(Validated.validNec[Err, Chain[(I, Ast[S])]](Chain.nil))(_ combine _))
      )

  def resolveImports(id: I, ast: Ast[S]): F[ValidatedNec[Err, Map[I, Err]]] =
    ast.head.tailForced
      .map(_.head)
      .collect { case ImportExpr(filename) =>
        sources
          .resolve(id, filename.value.drop(1).dropRight(1))
          .map(_.bimap(_.map(ResolveImportsErr(id, filename, _)), Chain.one))
      }
      .traverse(identity)
      .map(_.foldLeft(Validated.validNec[Err, Chain[I]](Chain.empty))(_ combine _))

  def sourceModules: F[ValidatedNec[Err, Modules[I, Err, T]]] =
    parseSources.flatMap {
      case Validated.Valid(srcs) =>
        srcs.traverse { case (id, ast) =>
          resolveImports(id, ast).map(_.map(AquaModule(id, _, transpile(ast))).map(Chain.one))
        }.map(
          _.foldLeft(Validated.validNec[Err, Chain[AquaModule[I, Err, T]]](Chain.empty))(
            _ combine _
          )
        )
      case Validated.Invalid(errs) =>
        Validated.invalid[NonEmptyChain[Err], Chain[AquaModule[I, Err, T]]](errs).pure[F]
    }.map(_.map(_.foldLeft(Modules[I, Err, T]())(_ add _)))

  def loadModules(ids: Map[I, Err]): F[ValidatedNec[Err, Chain[AquaModule[I, Err, T]]]] =
    ids.toList.traverse { case (id, err) => sources.load(id).map(_.bimap()) }

  def module(id: I, ast: Ast[S], dependsOn: Map[I, Err]): AquaModule[I, Err, T] =
    AquaModule(id, dependsOn, transpile(ast))
}
