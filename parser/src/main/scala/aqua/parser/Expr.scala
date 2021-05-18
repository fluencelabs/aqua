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

  trait ResultError[F[_]]
  case class ParserError[F[_]](err: P.Error) extends ResultError[F]
  case class IndentError[F[_]](indent: F[String], message: String) extends ResultError[F]

  trait Companion {

    def p[F[_]: LiftParser: Comonad]: P[Expr[F]]

    def readLine[F[_]: LiftParser: Comonad]: P[Expr[F]] = p
  }

  def defer(companion: => RootCompanion): RootCompanion = new RootCompanion {

    override def readLine[F[_]: LiftParser: Comonad]: P[Expr[F]] = companion.readLine[F]

    override def p[F[_]: LiftParser: Comonad]: P[Expr[F]] = companion.p[F]

    override def ast[F[_]: LiftParser: Comonad](): P[Either[ResultError[F], Ast.Tree[F]]] =
      companion.ast[F]()
  }

  // expression that could have children
  // that will be parsed by `ast` method to a tree
  trait RootCompanion extends Companion {
    def ast[F[_]: LiftParser: Comonad](): P[Either[ResultError[F], Ast.Tree[F]]]

    override def readLine[F[_]: LiftParser: Comonad]: P[Expr[F]] = p <* ` : `
  }

  // root expression that don't have children
  abstract class RootLeaf extends RootCompanion {

    override def ast[F[_]: LiftParser: Comonad](): P[Either[ResultError[F], Ast.Tree[F]]] =
      (` *`.with1 *> p[F]).map(e =>
        Right(
          Cofree[Chain, Expr[F]](
            e,
            Eval.now(Chain.empty)
          )
        )
      )
  }

  trait And extends RootCompanion {
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
      root: Option[(F[String], Expr[F])] = None,
      window: Chain[(F[String], Expr[F])] = Chain.empty,
      currentChildren: Chain[Ast.Tree[F]] = Chain.empty,
      error: Option[ResultError[F]] = None
    )

    // converts list of expressions to a tree
    def listToTree[F[_]: Comonad: LiftParser](
      head: Expr[F],
      exprs: Chain[(F[String], Expr[F])]
    ): Either[ResultError[F], Ast.Tree[F]] = {
      // if we don't have elements in a list, then head is a leaf
      exprs.headOption.fold[Either[ResultError[F], Cofree[Chain, Expr[F]]]](Right(leaf(head))) {
        lHead =>
          // size of an indentation
          val initialIndent = lHead._1.extract.length
          // recursively creating a tree
          // moving a window on a list depending on the nesting of the code
          val acc = exprs.foldLeft(
            Acc[F]()
          ) { case (acc, (i, currentExpr)) =>
            acc.error match {
              case None =>
                acc.root match {
                  case None =>
                    currentExpr match {
                      // if next is root companion, start to gather all expressions under this root
                      case e if e.root =>
                        acc.copy(root = Some(i, e))
                      // create leaf if token is on current level
                      case e =>
                        acc.copy(currentChildren = acc.currentChildren.append(leaf(e)))
                    }
                  // if we have root companion, gather all expressions that have indent > than current
                  case r @ Some((_, root)) =>
                    if (i.extract.length > initialIndent) {
                      Acc[F](
                        r,
                        acc.window.append((i, currentExpr)),
                        acc.currentChildren,
                        acc.error
                      )
                    } else if (i.extract.length == initialIndent) {
                      // if root have no tokens in it - return an error
                      if (acc.window.isEmpty) {
                        Acc(error = Some(IndentError(i, "Expression have no body")))
                      } else {
                        // create a tree from gathered expressions and continue
                        val treeE = listToTree[F](root, acc.window)

                        treeE match {
                          case Right(tree) =>
                            val withTree = acc.currentChildren.append(tree)
                            currentExpr match {
                              // if next expression is root companion, start to gather all tokens under this root
                              case e if e.root =>
                                acc.copy(
                                  root = Some(i, e),
                                  currentChildren = withTree,
                                  window = Chain.empty
                                )
                              // create leaf if token is on current level
                              case e =>
                                acc.copy(
                                  root = None,
                                  currentChildren = withTree.append(leaf(e)),
                                  window = Chain.empty
                                )
                            }
                          case Left(e) =>
                            // forward an error
                            acc.copy(error = Some(e))
                        }
                      }

                    } else {
                      Acc[F](error =
                        Some(
                          IndentError(
                            i,
                            "Wrong indentation. It must match the indentation of the previous expressions."
                          )
                        )
                      )
                    }
                }
              case e @ Some(_) =>
                acc.copy(error = e)
            }
          }

          // finalize all `tails` in the accumulator
          acc.error match {
            case None =>
              acc.root match {
                case Some((i, headExpr)) =>
                  if (acc.window.isEmpty) {
                    Left(IndentError(i, "Expression have no body"))
                  } else {
                    // create a tree from the last expressions if the window is not empty
                    // this may happen if a function ended in a nested expression
                    val tree = listToTree[F](headExpr, acc.window)
                    tree.map(t =>
                      Cofree[Chain, Expr[F]](head, Eval.now(acc.currentChildren.append(t)))
                    )
                  }
                case None =>
                  Right(Cofree[Chain, Expr[F]](head, Eval.now(acc.currentChildren)))
              }
            // pass through an error
            case Some(err) => Left(err)
          }

      }

    }

    override def ast[F[_]: LiftParser: Comonad](): P[Either[ResultError[F], Ast.Tree[F]]] =
      (p[F] ~ (` : \n+` *>
        (P.repSep(
          ` `.lift ~ P.oneOf(validChildren.map(_.readLine[F].backtrack)),
          ` \n+`
        ) <* ` \n`.?)))
        .map(t => listToTree(t._1, Chain.fromSeq(t._2.toList)))
  }
}
