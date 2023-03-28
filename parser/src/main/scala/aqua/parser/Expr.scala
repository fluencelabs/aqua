package aqua.parser

import aqua.parser.Ast.Tree
import aqua.parser.lexer.Token
import aqua.parser.lexer.Token.*
import aqua.parser.expr.func.ReturnExpr
import aqua.parser.lift.LiftParser.*
import aqua.parser.lift.Span.{P0ToSpan, PToSpan}
import aqua.parser.lift.{LiftParser, Span}
import cats.data.Chain.:==
import cats.data.{Chain, NonEmptyChain, Validated, ValidatedNec}
import cats.free.Cofree
import cats.parse.{Parser as P, Parser0 as P0}
import cats.syntax.comonad.*
import cats.{~>, Comonad, Eval}

abstract class Expr[F[_]](val companion: Expr.Companion, val token: Token[F]) {

  lazy val isBlock: Boolean = companion match {
    case _: Expr.Block => true
    case _ => false
  }

  def mapK[K[_]: Comonad](fk: F ~> K): Expr[K]
}

object Expr {

  trait Companion {

    def ast: P[ValidatedNec[ParserError[Span.S], Ast.Tree[Span.S]]]

  }

  trait Lexem extends Companion {
    def p: P[Expr[Span.S]]

    def readLine: P[Ast.Tree[Span.S]] =
      p.map(Cofree[Chain, Expr[Span.S]](_, Eval.now(Chain.empty)))
  }

  trait Leaf extends Lexem {

    override def ast: P[ValidatedNec[ParserError[Span.S], Tree[Span.S]]] =
      p.map(e =>
        Validated.validNec(
          Cofree[Chain, Expr[Span.S]](
            e,
            Eval.now(Chain.empty)
          )
        )
      )
  }

  class LazyLexem(companion: => Lexem) extends Lexem {
    lazy val c: Lexem = companion

    override def readLine: P[Ast.Tree[Span.S]] = c.readLine

    override def p: P[Expr[Span.S]] = c.p

    override def ast: P[ValidatedNec[ParserError[Span.S], Ast.Tree[Span.S]]] =
      c.ast
  }

  def defer(companion: => Lexem): Lexem = new LazyLexem(companion)

  // expression that could have children
  // that will be parsed by `ast` method to a tree
  trait Block extends Lexem {

    override def readLine: P[Ast.Tree[Span.S]] = super.readLine <* ` : `
  }

  abstract class Prefix(sep: P0[Any] = ` `) extends Lexem {
    def continueWith: List[Lexem]

    override def readLine: P[Ast.Tree[Span.S]] =
      ((super.readLine <* sep) ~ P.oneOf(continueWith.map(_.readLine.backtrack))).map {
        case (h, t) =>
          h.copy(tail = Eval.now(Chain.one(t)))
      }

    override def ast: P[ValidatedNec[ParserError[Span.S], Tree[Span.S]]] =
      ((super.readLine <* sep) ~ P.oneOf(continueWith.map(_.ast.backtrack))).map { case (h, tm) =>
        tm.map(t => h.copy(tail = Eval.now(Chain.one(t))))
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

    // Check if expression can be added in current block
    private def canAddToBlock[F[_]](block: Tree[F], expr: Expr[F]): Boolean = {
      println("block check: " + block)
      println("expr check: " + expr)
      block.head.companion match {
        case b: AndIndented =>
          b.validChildren.map {
            case ll: LazyLexem => ll.c
            case vc => vc
          }.contains(expr.companion)

        case _: Prefix =>
          block.tail.value.headOption.exists(t => canAddToBlock(t, expr))
        case _ => false
      }
    }

    // Generate error if expression (child) cannot be added to a block
    private def wrongChildError[F[_]](indent: F[String], expr: Expr[F]): ParserError[F] = {
      val msg = expr match {
        case ReturnExpr(_) =>
          "Return expression must be on the top indentation level and at the end of function body"
        // could there be other expressions?
        case _ => "This expression is on the wrong indentation level"
      }
      BlockIndentError(indent, msg)
    }

    private def headIsBlock[F[_]](tree: Tree[F]): Boolean = {
      tree.tail.value.headOption.exists(_.head.isBlock)
    }

    case class Acc[F[_]](
      block: Chain[(F[String], Tree[F])] = Chain.empty[(F[String], Tree[F])],
      window: Chain[(F[String], Tree[F])] = Chain.empty[(F[String], Tree[F])],
      currentChildren: Chain[Ast.Tree[F]] = Chain.empty[Ast.Tree[F]],
      error: Chain[ParserError[F]] = Chain.empty[ParserError[F]]
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
          val acc = exprs.foldLeft[Acc[F]](
            Acc[F]()
          ) {
            case (acc, (indent, currentExpr)) if acc.error.isEmpty =>
              println("block2: " + acc.block)
              println("current: " + last(currentExpr))
              acc.block.lastOption match {

                case None =>
                  val current = last(currentExpr)
                  current match {
                    // if next is block companion, start to gather all expressions under this block
                    case block if block.isBlock =>
                      println(s"add $block as block")
                      acc.copy(block = acc.block :+ (indent -> currentExpr))
                    // create leaf if token is on current level
                    case b =>
                      b.companion match {
                        // if prefix, then check if a child is a block and gather
                        case _: Prefix if headIsBlock(currentExpr) =>
                          println(s"add $b as block")
                          acc.copy(block = acc.block :+ (indent -> currentExpr))
                        case _ =>
                          acc.copy(currentChildren = acc.currentChildren.append(currentExpr))
                      }

                  }
                // if we have root companion, gather all expressions that have indent > than current
                case r @ Some((_, block)) =>
                  println("indent to initial indent: " + (indent.extract.length > initialIndent))
                  if (indent.extract.length > initialIndent) {
                    Acc[F](
                      acc.block,
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
                              println("delete last")
                              acc.copy(
                                block = acc.block.initLast.map(_._1).getOrElse(Chain.empty) :+ (indent -> currentExpr),
                                currentChildren = withTree,
                                window = Chain.empty
                              )
                            case b =>
                              b.companion match {
                                case _: Prefix if headIsBlock(currentExpr) =>
                                  println("delete last")
                                  acc.copy(
                                    block = acc.block.initLast.map(_._1).getOrElse(Chain.empty) :+ (indent -> currentExpr),
                                    currentChildren = withTree,
                                    window = Chain.empty
                                  )
                                // create leaf if a token is on current level
                                case _ =>
                                  // We cannot add some expressions to some blocks,
                                  // ie cannot add ReturnExpr to all blocks, except for the ArrowExpr
                                  if (!canAddToBlock(block, currentExpr.head))
                                    Acc(error = Chain.one(wrongChildError(indent, currentExpr.head)))
                                  else
                                    acc.copy(
                                      block = acc.block,
                                      currentChildren = withTree.append(currentExpr),
                                      window = Chain.empty
                                    )
                              }
                          }
                        }
                      )
                    }

                  } else {
                    "".toInt
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
              acc.block.lastOption match {
                case Some((i, headExpr)) =>
                  if (acc.window.isEmpty) {
                    println("azaza")
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

    override lazy val ast: P[ValidatedNec[ParserError[Span.S], Ast.Tree[Span.S]]] =
      (readLine ~ (` \n+` *>
        (P.repSep(
          ` `.lift ~ P.oneOf(validChildren.map(_.readLine.backtrack)),
          ` \n+`
        ) <* ` \n`.?)))
        .map(t => listToTree(t._1, Chain.fromSeq(t._2.toList)))
  }
}
