package aqua.parser

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

  trait Companion {

    def p[F[_]: LiftParser: Comonad]: P[Expr[F]]

    def readLine[F[_]: LiftParser: Comonad]: P[Expr[F]] = p

    def ast[F[_]: LiftParser: Comonad](): P[Either[P.Error, Ast.Tree[F]]]
  }

  def defer(companion: => Companion): Companion = new Companion {

    override def readLine[F[_]: LiftParser: Comonad]: P[Expr[F]] = companion.readLine[F]

    override def p[F[_]: LiftParser: Comonad]: P[Expr[F]] = companion.p[F]

    override def ast[F[_]: LiftParser: Comonad](): P[Either[P.Error, Ast.Tree[F]]] =
      companion.ast[F]()
  }

  trait RootCompanion extends Companion {

    override def readLine[F[_]: LiftParser: Comonad]: P[Expr[F]] = p <* ` : `
  }

  abstract class Leaf extends Companion {

    override def ast[F[_]: LiftParser: Comonad](): P[Either[P.Error, Ast.Tree[F]]] =
      (` *`.with1 ~ p[F]).map(e =>
        Right(
          Cofree[Chain, Expr[F]](
            e._2,
            Eval.now(Chain.empty)
          )
        )
      )
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

    case class Acc[F[_]](
      root: Option[Expr[F]] = None,
      window: Chain[(F[String], Expr[F])] = Chain.empty,
      currentChildren: Chain[Ast.Tree[F]] = Chain.empty,
      error: Option[P.Error] = None
    )

    def listToTree[F[_]: Comonad: LiftParser](
      head: Expr[F],
      exprs: Chain[(F[String], Expr[F])]
    ): Either[P.Error, Ast.Tree[F]] = {
      // if we don't have elements in a list, then head is a leaf
      exprs.headOption.fold[Either[P.Error, Cofree[Chain, Expr[F]]]](Right(leaf(head))) { lHead =>
        val start = lHead._1.extract.length
        val children = exprs.foldLeft(
          Acc[F]()
        ) { case (acc, (i, currentExpr)) =>
          acc.error match {
            case None =>
              acc.root match {
                case None =>
                  currentExpr match {
                    // if next is root companion, start to gather all tokens under this root
                    case e if e.root =>
                      acc.copy(root = Some(e))
                    // create leaf if token is on current level
                    case e =>
                      acc.copy(currentChildren = acc.currentChildren.append(leaf(e)))
                  }
                // if we have root companion, gather all tokens that have indent > than current
                case Some(root) =>
                  if (i.extract.length > start) {
                    Acc[F](
                      Some(root),
                      acc.window.append((i, currentExpr)),
                      acc.currentChildren,
                      acc.error
                    )
                  } else if (i.extract.length == start) {
                    // create a tree from gathered tokens and continue
                    val treeE = listToTree[F](root, acc.window)

                    // if window is empty - error

                    treeE match {
                      case Right(tree) =>
                        val withTree = acc.currentChildren.append(tree)
                        currentExpr match {
                          // if next is root companion, start to gather all tokens under this root
                          case e if e.root =>
                            acc.copy(root = Some(e), currentChildren = withTree)
                          // create leaf if token is on current level
                          case e =>
                            acc.copy(root = None, currentChildren = withTree.append(leaf(e)))
                        }
                      case Left(e) =>
                        // forward an error
                        acc.copy(error = Some(e))
                    }
                  } else {
                    // add an error
                    Acc[F](None, Chain.empty, acc.currentChildren, None)
                  }
              }
            case e @ Some(_) =>
              acc.copy(error = e)
          }

        }
        children.root match {
          case Some(headExpr) =>
            val tree = listToTree[F](headExpr, children.window)
            tree.map(t =>
              Cofree[Chain, Expr[F]](head, Eval.now(children.currentChildren.append(t)))
            )

          case None =>
            Right(Cofree[Chain, Expr[F]](head, Eval.now(children.currentChildren)))
        }

      }

    }

    override def ast[F[_]: LiftParser: Comonad](): P[Either[P.Error, Ast.Tree[F]]] =
      (p[F] ~ (` : \n+` *>
        (P.repSep(
          ` `.lift ~ P.oneOf(validChildren.map(_.readLine[F].backtrack)),
          ` \n+`
        ) <* ` \n`.?)))
        .map(t => listToTree(t._1, Chain.fromSeq(t._2.toList)))
  }
}
