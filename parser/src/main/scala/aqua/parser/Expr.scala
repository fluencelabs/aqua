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
import aqua.parser.lexer.Token
import aqua.parser.lexer.Token.*
import aqua.parser.lift.LiftParser.*
import aqua.parser.lift.Span.{given, *}
import aqua.parser.lift.{LiftParser, Span}

import cats.Show
import cats.data.Chain.:==
import cats.data.Validated.{invalid, invalidNec, invalidNel, valid, validNec, validNel}
import cats.data.{Chain, NonEmptyChain, Validated, ValidatedNec}
import cats.free.Cofree
import cats.parse.{Parser as P, Parser0 as P0}
import cats.syntax.comonad.*
import cats.{Comonad, Eval, ~>}
import scribe.Logging

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

  abstract class AndIndented extends Block with Logging {
    def validChildren: List[Lexem]

    override lazy val ast: P[ValidatedNec[ParserError[Span.S], Ast.Tree[Span.S]]] =
      (readLine ~ (` \n+` *>
        (P.repSep(
          ` `.lift ~ P.oneOf(validChildren.map(_.readLine.backtrack)),
          ` \n+`
        ) <* ` \n`.?))).map { case (open, lines) =>
        lines
          .foldLeft(
            ListToTreeConverter(open)
          ) { case (acc, (indent, line)) =>
            acc.next(indent, line)
          }
          .result
      }
  }

  given [S[_]]: Show[Expr[S]] with {
    // TODO: Make it better
    def show(e: Expr[S]): String = e.toString
  }
}
