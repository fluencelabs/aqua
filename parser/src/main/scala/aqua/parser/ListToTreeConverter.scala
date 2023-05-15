package aqua.parser

import aqua.parser.Ast.Tree
import aqua.parser.Expr.{AndIndented, LazyLexem, Prefix}
import aqua.parser.expr.func.ReturnExpr
import aqua.parser.lift.LiftParser

import cats.Comonad
import cats.data.Chain
import cats.data.Chain.:==
import cats.syntax.comonad.*
import cats.data.{NonEmptyChain, ValidatedNec}
import cats.data.Validated.*
import aqua.parser.Expr.LazyLexem

final case class ListToTreeConverter[F[_]](
  currentBlock: ListToTreeConverter.Block[F],
  stack: List[ListToTreeConverter.Block[F]] = Nil,
  errors: Chain[ParserError[F]] = Chain.empty[ParserError[F]]
)(using Comonad[F], LiftParser[F]) {
  import ListToTreeConverter.*

  private def addError(error: ParserError[F]): ListToTreeConverter[F] =
    copy(errors = errors :+ error)

  private def pushBlock(indent: F[String], line: Tree[F]): ListToTreeConverter[F] =
    copy(currentBlock = Block(indent, line), stack = currentBlock :: stack)

  private def addToCurrentBlock(line: Tree[F]): ListToTreeConverter[F] =
    copy(currentBlock = currentBlock.add(line))

  private def popBlock: Option[ListToTreeConverter[F]] =
    stack match {
      case Nil => None
      case prevBlock :: tail =>
        Some(copy(currentBlock = prevBlock.add(currentBlock.close), stack = tail))
    }

  @scala.annotation.tailrec
  def next(indent: F[String], line: Tree[F]): ListToTreeConverter[F] =
    if (indentValue(indent) > indentValue(currentBlock.indent)) {
      if (isBlock(line)) {
        pushBlock(indent, line)
      } else {
        val expr = lastExpr(line)

        if (currentBlock.canAdd(expr)) {
          addToCurrentBlock(line)
        } else {
          addError(wrongChildError(indent, expr))
        }
      }
    } else {
      val emptyChecked = if (currentBlock.isEmpty) {
        addError(emptyBlockError(currentBlock.indent))
      } else this

      emptyChecked.popBlock match {
        case Some(blockPopped) => blockPopped.next(indent, line)
        case _ => emptyChecked.addError(unexpectedIndentError(indent))
      }
    }

  @scala.annotation.tailrec
  def result: ValidatedNec[ParserError[F], Tree[F]] =
    popBlock match {
      case Some(blockPopped) => blockPopped.result
      case _ =>
        NonEmptyChain
          .fromChain(errors)
          .map(invalid)
          .getOrElse(
            valid(currentBlock.close)
          )
    }
}

object ListToTreeConverter {

  def apply[F[_]](open: Tree[F])(using Comonad[F], LiftParser[F]): ListToTreeConverter[F] =
    ListToTreeConverter(Block(open.head.token.as(""), open))

  final case class Block[F[_]](
    indent: F[String],
    block: Tree[F],
    content: Chain[Tree[F]] = Chain.empty[Tree[F]]
  ) {

    def canAdd(expr: Expr[F]): Boolean = {
      def checkFor(tree: Tree[F]): Boolean =
        tree.head.companion match {
          case indented: AndIndented =>
            indented.validChildren.map {
              case ll: LazyLexem => ll.c
              case vc => vc
            }.contains(expr.companion)
          case _: Prefix =>
            tree.tail.value.headOption.exists(checkFor)
          case _ => false
        }

      checkFor(block)
    }

    def add(child: Tree[F]): Block[F] =
      copy(content = content :+ child)

    def isEmpty: Boolean = content.isEmpty

    def close: Tree[F] = {
      def setLast(tree: Tree[F], children: Chain[Tree[F]]): Tree[F] =
        tree.copy(tail = tree.tail.map {
          case init :== last =>
            init :+ setLast(last, children)
          case _ =>
            children
        })

      setLast(block, content)
    }
  }

  private def indentValue[F[_]](indent: F[String])(using Comonad[F]): Int =
    indent.extract.length

  private def isBlock[F[_]](line: Tree[F]): Boolean =
    line.tail.value.headOption
      .map(_.head.isBlock)
      .getOrElse(line.head.isBlock)

  @scala.annotation.tailrec
  private def lastExpr[F[_]](tree: Tree[F]): Expr[F] =
    tree.tailForced.lastOption match {
      case Some(t) => lastExpr(t)
      case _ => tree.head
    }

  def wrongChildError[F[_]](indent: F[String], expr: Expr[F]): ParserError[F] = {
    val msg = expr match {
      case ReturnExpr(_) =>
        "Return expression must be on the top indentation level and at the end of function body"
      // could there be other expressions?
      case _ => "This expression is on the wrong indentation level"
    }
    BlockIndentError(indent, msg)
  }

  def emptyBlockError[F[_]](indent: F[String]): ParserError[F] =
    BlockIndentError(indent, "Block expression has no body")

  def unexpectedIndentError[F[_]](indent: F[String]): ParserError[F] =
    BlockIndentError(indent, "Unexpected indentation")

}
