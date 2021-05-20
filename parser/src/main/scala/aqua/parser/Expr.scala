package aqua.parser

import aqua.parser.Ast.Tree
import aqua.parser.lexer.Token._
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser._
import cats.data.{Chain, NonEmptyChain, Validated, ValidatedNec}
import cats.free.Cofree
import cats.parse.{Parser => P}
import cats.syntax.comonad._
import cats.{Comonad, Eval}
import Chain.:==

abstract class Expr[F[_]](val companion: Expr.Companion) {

  lazy val isBlock: Boolean = companion match {
    case _: Expr.Block => true
    case _ => false
  }
}

object Expr {

  trait Companion {

    def ast[F[_]: LiftParser: Comonad](): P[ValidatedNec[ParserError[F], Ast.Tree[F]]]

  }

  trait Lexem extends Companion {
    def p[F[_]: LiftParser: Comonad]: P[Expr[F]]

    def readLine[F[_]: LiftParser: Comonad]: P[Ast.Tree[F]] =
      p.map(Cofree[Chain, Expr[F]](_, Eval.now(Chain.empty)))
  }

  trait Leaf extends Lexem {

    override def ast[F[_]: LiftParser: Comonad](): P[ValidatedNec[ParserError[F], Tree[F]]] =
      p[F].map(e =>
        Validated.validNec(
          Cofree[Chain, Expr[F]](
            e,
            Eval.now(Chain.empty)
          )
        )
      )
  }

  def defer(companion: => Lexem): Lexem = new Lexem {
    private lazy val c = companion

    override def readLine[F[_]: LiftParser: Comonad]: P[Ast.Tree[F]] = c.readLine[F]

    override def p[F[_]: LiftParser: Comonad]: P[Expr[F]] = c.p[F]

    override def ast[F[_]: LiftParser: Comonad](): P[ValidatedNec[ParserError[F], Ast.Tree[F]]] =
      c.ast[F]()
  }

  // expression that could have children
  // that will be parsed by `ast` method to a tree
  trait Block extends Lexem {

    override def readLine[F[_]: LiftParser: Comonad]: P[Ast.Tree[F]] = super.readLine[F] <* ` : `
  }

  trait Prefix extends Lexem {
    def continueWith: List[Lexem]

    override def readLine[F[_]: LiftParser: Comonad]: P[Ast.Tree[F]] =
      ((super.readLine[F] <* ` `) ~ P.oneOf(continueWith.map(_.readLine.backtrack))).map {
        case (h, t) => h.copy(tail = Eval.now(Chain.one(t)))
      }

    override def ast[F[_]: LiftParser: Comonad](): P[ValidatedNec[ParserError[F], Tree[F]]] =
      ((super.readLine[F] <* ` `) ~ P.oneOf(continueWith.map(_.ast().backtrack))).map {
        case (h, tm) => tm.map(t => h.copy(tail = Eval.now(Chain.one(t))))
      }
  }

  abstract class AndIndented extends Block {
    def validChildren: List[Lexem]

    private def leaf[F[_]](expr: Expr[F]): Ast.Tree[F] =
      Cofree[Chain, Expr[F]](
        expr,
        Eval.now(Chain.empty)
      )

    private def last[F[_]](tree: Ast.Tree[F]): Expr[F] =
      tree.tailForced.lastOption.fold(tree.head)(last)

    private def setLeafs[F[_]](tree: Ast.Tree[F], children: Chain[Tree[F]]): Tree[F] =
      tree.copy(tail = tree.tail.map {
        case pref :== last =>
          pref :+ setLeafs(last, children)
        case _ =>
          children
      })

    case class Acc[F[_]](
      block: Option[(F[String], Tree[F])] = None,
      window: Chain[(F[String], Tree[F])] = Chain.empty,
      currentChildren: Chain[Ast.Tree[F]] = Chain.empty,
      error: Chain[ParserError[F]] = Chain.empty
    )

    // converts list of expressions to a tree
    def listToTree[F[_]: Comonad: LiftParser](
      head: Tree[F],
      exprs: Chain[(F[String], Ast.Tree[F])]
    ): ValidatedNec[ParserError[F], Ast.Tree[F]] = {
      // if we don't have elements in a list, then head is a leaf
      exprs.headOption
        .fold[ValidatedNec[ParserError[F], Ast.Tree[F]]](Validated.validNec(head)) { lHead =>
          // size of an indentation
          val initialIndent = lHead._1.extract.length
          // recursively creating a tree
          // moving a window on a list depending on the nesting of the code
          val acc = exprs.foldLeft(
            Acc[F]()
          ) {
            case (acc, (indent, currentExpr)) if acc.error.isEmpty =>
              acc.block match {
                case None =>
                  last(currentExpr) match {
                    // if next is block companion, start to gather all expressions under this block
                    case block if block.isBlock =>
                      acc.copy(block = Some(indent -> currentExpr))
                    // create leaf if token is on current level
                    case e =>
                      acc.copy(currentChildren = acc.currentChildren.append(leaf(e)))
                  }
                // if we have root companion, gather all expressions that have indent > than current
                case r @ Some((_, block)) =>
                  if (indent.extract.length > initialIndent) {
                    Acc[F](
                      r,
                      acc.window.append((indent, currentExpr)),
                      acc.currentChildren,
                      acc.error
                    )
                  } else if (indent.extract.length == initialIndent) {
                    // if root have no tokens in it - return an error
                    if (acc.window.isEmpty) {
                      Acc(error =
                        Chain.one(BlockIndentError(indent, "Block expression has no body"))
                      )
                    } else {
                      // create a tree from gathered expressions and continue
                      listToTree[F](block, acc.window).fold(
                        e => acc.copy(error = e.toChain),
                        tree => {
                          val withTree = acc.currentChildren.append(tree)
                          last(currentExpr) match {
                            // if next expression is root companion, start to gather all tokens under this root
                            case block if block.isBlock =>
                              acc.copy(
                                block = Some(indent -> currentExpr),
                                currentChildren = withTree,
                                window = Chain.empty
                              )
                            // create leaf if token is on current level
                            case e =>
                              acc.copy(
                                block = None,
                                currentChildren = withTree.append(leaf(e)),
                                window = Chain.empty
                              )
                          }
                        }
                      )
                    }

                  } else {
                    Acc[F](error =
                      Chain.one(
                        BlockIndentError(
                          indent,
                          "Wrong indentation. It must match the indentation of the previous expressions."
                        )
                      )
                    )
                  }
              }
            case (acc, _) =>
              acc
          }

          // finalize all `tails` in the accumulator
          NonEmptyChain.fromChain(acc.error) match {
            case None =>
              acc.block match {
                case Some((i, headExpr)) =>
                  if (acc.window.isEmpty) {
                    Validated.invalidNec(BlockIndentError(i, "Block expression has no body"))
                  } else {
                    // create a tree from the last expressions if the window is not empty
                    // this may happen if a function ended in a nested expression
                    val tree = listToTree[F](headExpr, acc.window)
                    tree.map(t => setLeafs(head, acc.currentChildren :+ t))
                  }
                case None =>
                  Validated.validNec(setLeafs(head, acc.currentChildren))
              }
            // pass through an error
            case Some(err) => Validated.invalid(err)
          }

        }

    }

    override def ast[F[_]: LiftParser: Comonad](): P[ValidatedNec[ParserError[F], Ast.Tree[F]]] =
      (readLine[F] ~ (` \n+` *>
        (P.repSep(
          ` `.lift ~ P.oneOf(validChildren.map(_.readLine[F].backtrack)),
          ` \n+`
        ) <* ` \n`.?)))
        .map(t => listToTree(t._1, Chain.fromSeq(t._2.toList)))
  }
}
