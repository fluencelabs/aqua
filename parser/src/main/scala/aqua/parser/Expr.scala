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
import cats.data.Validated.{invalid, invalidNec, invalidNel, valid, validNec, validNel}
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
      println("expr to add: " + expr)
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
      println("what is block111?: " + tree)
      println("what is block?: " + tree.tail.value.headOption)
      tree.tail.value.headOption match {
        case Some(t) => t.head.isBlock
        case _ => tree.head.isBlock
      }
    }

    case class Acc[F[_]](
      block: Tree[F],
      initialIndentF: F[String],
      tail: Chain[(F[String], Ast.Tree[F])] = Chain.empty[(F[String], Ast.Tree[F])],
      window: Chain[Tree[F]] = Chain.empty[Tree[F]],
      error: Chain[ParserError[F]] = Chain.empty[ParserError[F]]
    ) {

      override def toString: String = {
        s"""Acc(
           |    block = $block,
           |    window = ${window.toList.mkString("\n")},
           |    tail = $tail,
           |    error = $error,
           |)""".stripMargin
      }
    }

    // converts list of expressions to a tree
    def listToTree[F[_]: Comonad: LiftParser](
      acc: Acc[F]
    ): ValidatedNec[ParserError[F], (Ast.Tree[F], Chain[Ast.Tree[F]], Chain[(F[String], Ast.Tree[F])])] = {
      println("HEAD: " + acc.block.forceTail)
      // if we don't have elements in a list, then head is a leaf

      val initialIndent = acc.initialIndentF.extract.length

      acc.tail.uncons match {
        case Some(((currentIndent, currentExpr), tail)) =>
          val current = last(currentExpr)
          println("current: " + current)
          println("current is block: " + headIsBlock(currentExpr))
          if (currentIndent.extract.length > initialIndent) {
            if (headIsBlock(currentExpr)) {
              println("NEW BLOCK: " + currentExpr)
              listToTree(Acc(currentExpr, currentIndent, tail)).andThen { case (innerTree, window, newTail) =>
                if (window.nonEmpty) {
                  println("WINDOW NON EMPTY, WARNING")
                }
                listToTree(acc.copy(window = acc.window :+ innerTree, tail = newTail))
              }
            } else {
              // add to window
              if (canAddToBlock(acc.block, current)) {
                listToTree(acc.copy(window = acc.window :+ currentExpr, tail = tail))
              } else {
                invalidNec(wrongChildError(currentIndent, current))
              }

            }
          } else if (currentIndent.extract.length == initialIndent) {
            // add window to head and refresh
            println("processed head: " + acc.block.forceTail)
            println("window: " + acc.window)
            validNec((setLeafs(acc.block, acc.window), Chain.empty, (currentIndent, currentExpr) +: tail))
          } else {
            invalidNec(wrongChildError(currentIndent, current))
          }

        case None =>
          // end of top-level block
          validNec((setLeafs(acc.block, acc.window), Chain.empty, Chain.empty))
      }
    }

    override lazy val ast: P[ValidatedNec[ParserError[Span.S], Ast.Tree[Span.S]]] =
      (readLine ~ (` \n+` *>
        (P.repSep(
          ` `.lift ~ P.oneOf(validChildren.map(_.readLine.backtrack)),
          ` \n+`
        ) <* ` \n`.?))).map { t =>
        val startIndent = t._1.head.token.as("")
        println("t1: " + t._1.head)
        listToTree(Acc(t._1, startIndent, Chain.fromSeq(t._2.toList))).map { res =>
          println("this must be empty: " + res._2)
          println("result: " + res._1.forceTail)
          res._1
        }
      }
  }
}
