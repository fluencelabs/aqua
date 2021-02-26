package aqua.parser.lift

import cats.Comonad
import cats.parse.{LocationMap, Parser => P}

import scala.language.implicitConversions

case class Span(startIndex: Int, endIndex: Int) {

  def focus(text: String, ctx: Int): Option[Span.Focus] = {
    val map = LocationMap(text)
    map.toLineCol(startIndex).flatMap {
      case (line, column) =>
        map
          .getLine(line)
          .map(l =>
            Span.Focus(
              (Math.max(0, line - ctx) until line).map(map.getLine).toList.flatten, {
                val (l1, l2) = l.splitAt(column)
                val (lc, l3) = l2.splitAt(endIndex - startIndex)
                (l1, lc, l3)
              },
              ((line + 1) to (line + ctx)).map(map.getLine).toList.flatten
            )
          )
    }
  }
}

object Span {

  case class Focus(pre: List[String], line: (String, String, String), post: List[String]) {

    def toConsoleStr(onLeft: String, onRight: String = Console.RESET): String =
      pre.mkString("\n") +
        "\n" +
        line._1 +
        onLeft +
        line._2 +
        onRight +
        line._3 +
        "\n" +
        (" " * line._1.length) +
        onLeft +
        ("^" * line._2.length) +
        ("=" * line._3.length) +
        onRight +
        "\n" +
        post.mkString("\n")
  }

  type F[T] = (Span, T)

  implicit object spanComonad extends Comonad[F] {
    override def extract[A](x: F[A]): A = x._2

    override def coflatMap[A, B](fa: F[A])(f: F[A] ⇒ B): F[B] = fa.copy(_2 = f(fa))

    override def map[A, B](fa: F[A])(f: A ⇒ B): F[B] = fa.copy(_2 = f(fa._2))
  }

  implicit object spanLiftParser extends LiftParser[F] {

    override def lift[T](p: P[T]): P[F[T]] =
      (P.index.with1 ~ p ~ P.index).map {
        case ((s, v), e) ⇒ (Span(s, e), v)
      }
  }

}
