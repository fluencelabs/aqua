package aqua.parser

import aqua.parser.Ast.Tree
import aqua.parser.lexer.Token._
import aqua.parser.lift.LiftParser
import cats.data.Chain
import cats.free.Cofree
import cats.parse.{Parser => P}
import cats.{Comonad, Eval}

trait Expr[F[_]]

object Expr {

  def defer(companion: => Companion): Companion = new Companion {
    override def p[F[_]: LiftParser: Comonad]: P[Expr[F]] = companion.p[F]

    override def ast[F[_]: LiftParser: Comonad](ps: Indent): P[Tree[F]] = companion.ast[F](ps)
  }

  trait Companion {
    def p[F[_]: LiftParser: Comonad]: P[Expr[F]]

    def ast[F[_]: LiftParser: Comonad](ps: Indent): P[Ast.Tree[F]]
  }

  abstract class And(thenInline: List[Expr.Companion], orIndented: List[Expr.Companion]) extends Companion {

    override def ast[F[_]: LiftParser: Comonad](ps: Indent): P[Ast.Tree[F]] =
      (p[F] ~ ((` `.backtrack *> P
        .oneOf(thenInline.map(_.ast[F](ps).backtrack))
        .map(Chain.one)) | (` : \n+` *> indented(
        s => {
          val psI = ps.copy(indent = s)
          P.oneOf(orIndented.map(_.ast[F](psI).backtrack))
        },
        ps.indent
      )).map(_.toList).map(Chain.fromSeq))).map { case (expr, internal) =>
        Cofree[Chain, Expr[F]](expr, Eval.now(internal))
      }
  }

  abstract class Leaf extends Companion {

    override def ast[F[_]: LiftParser: Comonad](ps: Indent): P[Ast.Tree[F]] =
      p[F].map(Cofree[Chain, Expr[F]](_, Eval.now(Chain.empty)))
  }

  abstract class AndThen(headExpr: Companion, oneOfExprs: Companion*)
      extends And(thenInline = headExpr :: oneOfExprs.toList, orIndented = Nil)

  abstract class AndIndented(headExpr: Companion, oneOfExprs: Companion*)
      extends And(thenInline = Nil, orIndented = headExpr :: oneOfExprs.toList)
}
