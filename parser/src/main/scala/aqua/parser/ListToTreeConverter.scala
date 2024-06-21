/*
 * Copyright (C) 2024  Fluence DAO
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

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

/**
 * Converts a list of lines to a tree
 */
final case class ListToTreeConverter[F[_]](
  currentBlock: ListToTreeConverter.Block[F], // Current block data
  stack: List[ListToTreeConverter.Block[F]] = Nil, // Stack of opened blocks
  errors: Chain[ParserError[F]] = Chain.empty[ParserError[F]] // Errors
)(using Comonad[F]) {

  // Import helper functions
  import ListToTreeConverter.*

  private def addError(error: ParserError[F]): ListToTreeConverter[F] =
    copy(errors = errors :+ error)

  private def pushBlock(indent: F[String], line: Tree[F]): ListToTreeConverter[F] =
    copy(currentBlock = Block(indent, line), stack = currentBlock :: stack)

  private def addToCurrentBlock(indent: F[String], line: Tree[F]): ListToTreeConverter[F] =
    copy(currentBlock = currentBlock.add(indent, line))

  private def popBlock: Option[ListToTreeConverter[F]] =
    stack match {
      case Nil => None
      case prevBlock :: tail =>
        Some(
          copy(
            currentBlock = prevBlock.add(
              currentBlock.indent,
              currentBlock.close
            ),
            stack = tail
          )
        )
    }

  /**
   * Method to call on each new line
   */
  @scala.annotation.tailrec
  def next(indent: F[String], line: Tree[F]): ListToTreeConverter[F] =
    currentBlock.classifyIndent(indent) match {
      case IndentRelation.Child(consistent) =>
        val consistentChecked = if (!consistent) {
          addError(inconsistentIndentError(indent))
        } else this

        if (isBlock(line)) {
          consistentChecked.pushBlock(indent, line)
        } else {
          val expr = lastExpr(line)

          if (currentBlock.isValidChild(expr)) {
            consistentChecked.addToCurrentBlock(indent, line)
          } else {
            consistentChecked.addError(wrongChildError(indent, expr))
          }
        }
      // Note:    this doesn't necessarly mean that indentation is correct
      //          next block will check it
      case IndentRelation.Sibling =>
        val emptyChecked = if (currentBlock.isEmpty) {
          addError(emptyBlockError(currentBlock.indent))
        } else this

        emptyChecked.popBlock match {
          case Some(blockPopped) => blockPopped.next(indent, line)
          // This should not happen because of the way of parsing
          case _ => emptyChecked.addError(unexpectedIndentError(indent))
        }
      case IndentRelation.Unexpected =>
        addError(unexpectedIndentError(indent))
    }

  /**
   * Produce the result of the conversion
   */
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

  /**
   * Constructs a converter from block opening line
   */
  def apply[F[_]](open: Tree[F])(using Comonad[F]): ListToTreeConverter[F] =
    ListToTreeConverter(Block(open.head.token.as(""), open))

  /**
   * Describes the realtion of next line to the current block
   */
  enum IndentRelation {
    case Child(consistent: Boolean)
    case Sibling
    case Unexpected
  }

  /**
   * Data associated with a block
   */
  final case class Block[F[_]](
    indent: F[String], // Indentation of the block opening line
    block: Tree[F], // Block opening line
    childIndent: Option[F[String]] = None, // Indentation of the first child
    content: Chain[Tree[F]] = Chain.empty[Tree[F]] // Children of the block
  ) {

    /**
     * Classify the next line relative to the block
     */
    def classifyIndent(lineIndent: F[String])(using Comonad[F]): IndentRelation = {
      val blockIndentStr = indent.extract
      val lineIndentStr = lineIndent.extract

      if (lineIndentStr.startsWith(blockIndentStr)) {
        lazy val consistentChild = childIndent
          .map(_.extract)
          .fold(
            lineIndentStr.length > blockIndentStr.length
          )(_ == lineIndentStr)

        if (lineIndentStr.length == blockIndentStr.length) {
          IndentRelation.Sibling
        } else {
          IndentRelation.Child(consistentChild)
        }
      } else if (blockIndentStr.startsWith(lineIndentStr)) {
        IndentRelation.Sibling
      } else {
        IndentRelation.Unexpected
      }
    }

    /**
     * Check if expr can be added to this block
     */
    def isValidChild(expr: Expr[F]): Boolean = {
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

    /**
     * Add line to the block
     */
    def add(indent: F[String], line: Tree[F]): Block[F] =
      copy(
        content = content :+ line,
        childIndent = childIndent.orElse(Some(indent))
      )

    /**
     * Check if the block has no children
     */
    def isEmpty: Boolean = content.isEmpty

    /**
     * Create a tree corresponding to the block
     */
    def close: Tree[F] = {

      /**
       * Set children of the rightmost expression in tree
       */
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

  // TODO(LNG-150): This way of counting indent does not play way with mixed tabs and spaces
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

  def inconsistentIndentError[F[_]](indent: F[String]): ParserError[F] =
    BlockIndentError(indent, "Inconsistent indentation in the block")

  def unexpectedIndentError[F[_]](indent: F[String]): ParserError[F] =
    BlockIndentError(indent, "Unexpected indentation")

}
