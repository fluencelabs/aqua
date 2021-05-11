package aqua.parser

import aqua.parser.expr._
import aqua.parser.lexer.Token._
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser._
import cats.data.Chain
import cats.free.Cofree
import cats.parse.{Parser => P}
import cats.syntax.comonad._
import cats.{Comonad, Eval}

trait Expr[F[_]] {
  def root = false
}

object Expr {

  val allChildrenExprs = List(
    ArrowTypeExpr,
    CallArrowExpr,
    FieldTypeExpr,
    DeclareStreamExpr,
    OnExpr,
    IfExpr,
    ReturnExpr,
    ForExpr,
    ElseOtherwiseExpr,
    CallArrowExpr,
    AbilityIdExpr
  )

  val rootExprs = List(
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

    override def readLine[F[_]: LiftParser: Comonad]: P[Expr[F]] = companion.readLine[F]

    override def p[F[_]: LiftParser: Comonad]: P[Expr[F]] = companion.p[F]

    override def ast[F[_]: LiftParser: Comonad](): P[Ast.Tree[F]] =
      companion.ast[F]()
  }

  trait RootCompanion extends Companion {

    override def readLine[F[_]: LiftParser: Comonad]: P[Expr[F]] = p <* ` : `
  }

  abstract class Leaf extends Companion {

    override def ast[F[_]: LiftParser: Comonad](): P[Ast.Tree[F]] =
      (` *`.with1 ~ p[F]).map { case (e) =>
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

    def leaf[F[_]](expr: Expr[F]): Ast.Tree[F] = {
      Cofree[Chain, Expr[F]](
        expr,
        Eval.now(Chain.empty)
      )
    }

    def listToTree[F[_]: Comonad](
      head: Expr[F],
      exprs: Chain[(F[String], Expr[F])]
    ): Ast.Tree[F] = {
      exprs.headOption.fold(leaf(head)) { lHead =>
        val start = lHead._1.extract.length
        val children = exprs.foldLeft(
          (Option.empty[Expr[F]], Chain.empty[(F[String], Expr[F])], Chain.empty[Ast.Tree[F]])
        ) { case ((h, window, currentLevel), (i, currentExpr)) =>
          h match {
            case None =>
              currentExpr match {
                // if next is root companion, start to gather all tokens under this root
                case e if e.root =>
                  (Some(e), Chain.empty, currentLevel)
                // create leaf if token is on current level
                case e =>
                  (None, Chain.empty, currentLevel.append(leaf(e)))
              }
            // if we have root companion, gather all tokens that have indent > than current
            case Some(he) =>
              if (i.extract.length > start) {
                (Some(he), window.append((i, currentExpr)), currentLevel)
              } else {
                // create a tree from gathered tokens and continue
                val tree = listToTree[F](he, window)
                val withTree = currentLevel.append(tree)
                currentExpr match {
                  // if next is root companion, start to gather all tokens under this root
                  case e if e.root =>
                    (Some(e), Chain.empty, withTree)
                  // create leaf if token is on current level
                  case e =>
                    (None, Chain.empty, withTree.append(leaf(e)))
                }
              }

          }
        }
        children._1 match {
          case Some(headExpr) =>
            val tree = listToTree[F](headExpr, children._2)
            Cofree[Chain, Expr[F]](head, Eval.now(children._3.append(tree)))
          case None =>
            Cofree[Chain, Expr[F]](head, Eval.now(children._3))
        }

      }

    }

    override def ast[F[_]: LiftParser: Comonad](): P[Ast.Tree[F]] = {
      (p[F] ~ (` : \n+` *>
        (P.repSep(
          ` `.lift ~ P.oneOf(validChildren.map(_.readLine[F].backtrack)),
          ` \n+`
        ) <* ` \n`.?)))
        .map(t => listToTree(t._1, Chain.fromSeq(t._2.toList)))
    }
  }
}
