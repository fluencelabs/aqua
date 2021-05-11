package aqua.parser

import aqua.parser.expr._
import aqua.parser.lexer.Token._
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser._
import cats.data.{Chain, NonEmptyList}
import cats.free.Cofree
import cats.parse.{Parser => P}
import cats.{Comonad, Eval}

trait Expr[F[_]]

object Expr {

  val allChildrenExprs = List(
    ArrowTypeExpr,
    CallArrowExpr,
    OnExpr,
    IfExpr,
    ForExpr,
    ElseOtherwiseExpr,
    FieldTypeExpr,
    ReturnExpr,
    CallArrowExpr,
    AbilityIdExpr,
    DeclareStreamExpr
  )

  val rootExprs = List(
    AliasExpr,
    ConstantExpr,
    DataStructExpr,
    FuncExpr,
    ServiceExpr,
    ForExpr,
    OnExpr,
    IfExpr,
    ElseOtherwiseExpr
  )

  trait Companion {
    def p[F[_]: LiftParser: Comonad]: P[Expr[F]]

    def readLine[F[_]: LiftParser: Comonad]: P[Expr[F]] = p

    def ast[F[_]: LiftParser: Comonad](): P[Ast.Tree[F]]
  }

  def defer(companion: => Companion): Companion = new Companion {
    override def p[F[_]: LiftParser: Comonad]: P[Expr[F]] = companion.p[F]

    override def ast[F[_]: LiftParser: Comonad](): P[Ast.Tree[F]] =
      companion.ast[F]()
  }

  trait RootCompanion extends Companion {

    override def readLine[F[_]: LiftParser: Comonad]: P[Expr[F]] = p <* ` : `
  }

  abstract class Leaf extends Companion {

    override def ast[F[_]: LiftParser: Comonad](): P[Ast.Tree[F]] =
      (` ` ~ p[F]).map { case (e) =>
        Cofree[Chain, Expr[F]](
          e._2,
          Eval.now(Chain.empty)
        )
      }
  }

  trait And extends Companion {
    def validChildren: List[Companion]
  }

  abstract class AndIndented extends And {

    def listToTree[F[_]](head: Expr[F], exprs: NonEmptyList[(F[String], Expr[F])]): Ast.Tree[F] = {
      val first = exprs.head._2
      Cofree[Chain, Expr[F]](first, Eval.now(Chain.empty))
    }

    override def ast[F[_]: LiftParser: Comonad](): P[Ast.Tree[F]] = {
      (p[F] ~ (` : \n+` *>
        (P.repSep(
          ` `.lift ~ P.oneOf(allChildrenExprs.map(_.readLine[F].backtrack)),
          ` \n+`
        ) <* ` \n`.?)))
        .map(t => listToTree(t._1, t._2))
    }
  }
}
