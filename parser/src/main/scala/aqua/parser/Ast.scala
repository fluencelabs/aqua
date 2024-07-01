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

import aqua.helpers.tree.Tree
import aqua.parser.head.HeaderExpr
import aqua.parser.lexer.Token

import cats.data.Chain
import cats.free.Cofree
import cats.syntax.flatMap.*
import cats.syntax.show.*
import cats.{Comonad, ~>}
import cats.{Eval, Show}

case class Ast[S[_]](head: Ast.Head[S], tree: Ast.Tree[S]) {

  def mapK[K[_]: Comonad](nt: S ~> K): Ast[K] =
    Ast(head.mapK(nt), tree.map(_.mapK(nt)))

  def cata[T](folder: (Expr[S], Chain[T]) => Eval[T]): Eval[T] =
    Cofree.cata[Chain, Expr[S], T](tree)(folder)
}

object Ast {

  final case class Head[S[_]](
    begin: Token[S],
    headers: Chain[HeaderExpr[S]]
  ) {

    def mapK[K[_]: Comonad](nt: S ~> K): Head[K] =
      copy(
        begin = begin.mapK(nt),
        headers = headers.map(_.mapK(nt))
      )

    def collect[T](pf: PartialFunction[HeaderExpr[S], T]): Chain[T] =
      headers.collect(pf)
  }

  type Tree[S[_]] = Cofree[Chain, Expr[S]]

  given [S[_]]: Show[Ast[S]] with {

    def show(ast: Ast[S]): String = {
      val head = ast.head.headers.map(_.show).toList.mkString("\n")
      val body = Tree.show(ast.tree)

      s"$head\n$body"
    }
  }
}
