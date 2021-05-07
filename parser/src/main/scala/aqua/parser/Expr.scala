package aqua.parser

import aqua.parser.lexer.Token._
import aqua.parser.lift.LiftParser
import cats.data.Chain
import cats.free.Cofree
import cats.parse.{Parser0, Parser => P}
import cats.{Comonad, Eval}

trait Expr[F[_]]

object Expr {

  trait Companion {
    def p[F[_]: LiftParser: Comonad]: P[Expr[F]]

    def indent: Parser0[String] = ` `.orElse(P.pure(""))

    def pI[F[_]: LiftParser: Comonad]: P[IndentExpr[F]] = (indent.with1 ~ p).map { case (i, e) =>
      IndentExpr(IndentSize.fromString(i), e)
    }

    def ast[F[_]: LiftParser: Comonad](): P[Ast.IndentTree[F]]
  }

  def defer(companion: => Companion): Companion = new Companion {
    override def p[F[_]: LiftParser: Comonad]: P[Expr[F]] = companion.p[F]

    override def ast[F[_]: LiftParser: Comonad](): P[Ast.IndentTree[F]] =
      companion.ast[F]()
  }

  abstract class Leaf extends Companion {

    override def ast[F[_]: LiftParser: Comonad](): P[Ast.IndentTree[F]] =
      pI[F].map { case (e) =>
        Cofree[Chain, IndentExpr[F]](
          e,
          Eval.now(Chain.empty)
        )
      }
  }

  trait And extends Companion {
    def validChildren: List[Companion]
  }

  abstract class AndThen extends And {

    override def ast[F[_]: LiftParser: Comonad](): P[Ast.IndentTree[F]] =
      (pI[F] ~ (` *` *> P
        .oneOf(validChildren.map(_.ast[F]()))
        .map(Chain.one))).map { case (expr, internal) =>
        Cofree[Chain, IndentExpr[F]](expr, Eval.now(internal))
      }
  }

  abstract class AndIndented extends And {

    override def ast[F[_]: LiftParser: Comonad](): P[Ast.IndentTree[F]] = {
      (pI[F] ~ (` : \n+` *>
        P.repSep(P.oneOf(validChildren.map(_.ast[F]())), ` \n`) <* ` \n`,
      )).map { case (expr, internal) =>
        val i = Chain.fromSeq(internal.toList)
        Cofree[Chain, IndentExpr[F]](expr, Eval.now(i))
      }
    }
  }
}
