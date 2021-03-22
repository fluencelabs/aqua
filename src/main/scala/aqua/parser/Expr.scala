package aqua.parser

import aqua.parser.Ast.Tree
import aqua.parser.lexer.Token._
import aqua.parser.lift.LiftParser
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

  abstract class Leaf extends Companion {

    override def ast[F[_]: LiftParser: Comonad](ps: Indent): P[Ast.Tree[F]] =
      p[F].map(Cofree[List, Expr[F]](_, Eval.now(Nil)))
  }

  abstract class AndThen(headExpr: Companion, oneOfExprs: Companion*) extends Companion {

    lazy val contents: List[Companion] = headExpr :: oneOfExprs.toList

    override def ast[F[_]: LiftParser: Comonad](ps: Indent): P[Ast.Tree[F]] =
      ((p[F] <* ` `) ~ P.oneOf(contents.map(_.ast[F](ps).backtrack))).map { case (expr, andThen) =>
        Cofree[List, Expr[F]](expr, Eval.now(andThen :: Nil))
      }
  }

  abstract class AndIndented(headExpr: Companion, oneOfExprs: Companion*) extends Companion {

    lazy val contents: List[Companion] = headExpr :: oneOfExprs.toList

    override def ast[F[_]: LiftParser: Comonad](ps: Indent): P[Ast.Tree[F]] =
      (p[F] ~ indented(
        s => {
          val psI = ps.copy(indent = s)
          P.oneOf(contents.map(_.ast[F](psI).backtrack))
        },
        ps.indent
      )).map { case (expr, internal) =>
        Cofree[List, Expr[F]](expr, Eval.now(internal.toList))
      }
  }
}
