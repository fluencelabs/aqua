package aqua.parser

case class Indent(indent: String = "") {
  def toSize: IndentSize = IndentSize(indent.length)
}
case class IndentSize(size: Int)

case class IndentExpr[F[_]](i: IndentSize, e: Expr[F])

object IndentSize {
  def fromString(string: String) = IndentSize(string.length)
}
