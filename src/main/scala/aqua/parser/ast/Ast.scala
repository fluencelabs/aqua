package aqua.parser.ast

import cats.free.Cofree

case class Ast[F[_]](tree: Ast.Tree[F]) {}

object Ast {
  type Tree[F[_]] = Cofree[List, Expr[F]]
}
