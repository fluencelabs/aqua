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
              (Math.max(0, line - ctx) until line).map(i => map.getLine(i).map(i -> _)).toList.flatten, {
                val (l1, l2) = l.splitAt(column)
                val (lc, l3) = l2.splitAt(endIndex - startIndex)
                (line, l1, lc, l3)
              },
              ((line + 1) to (line + ctx)).map(i => map.getLine(i).map(i -> _)).toList.flatten
            )
          )
    }
  }
}

object Span {

  case class Focus(pre: List[(Int, String)], line: (Int, String, String, String), post: List[(Int, String)]) {

    private lazy val lastN = post.lastOption.map(_._1).getOrElse(line._1) + 1
    private lazy val lastNSize = lastN.toString.length

    private def formatLine(l: (Int, String), onLeft: String, onRight: String) =
      formatLN(l._1, onLeft, onRight) + l._2

    private def formatLN(ln: Int, onLeft: String, onRight: String) = {
      val s = (ln + 1).toString
      onLeft + s + (" " * (lastNSize - s.length)) + onRight + " "
    }

    def toConsoleStr(msg: String, onLeft: String, onRight: String = Console.RESET): String =
      pre.map(formatLine(_, onLeft, onRight)).mkString("\n") +
        "\n" +
        formatLN(line._1, onLeft, onRight) +
        line._2 +
        onLeft +
        line._3 +
        onRight +
        line._4 +
        "\n" +
        (" " * (line._2.length + lastNSize + 1)) +
        onLeft +
        ("^" * line._3.length) +
        ("=" * line._4.length) +
        onRight +
        "\n" +
        (" " * (line._2.length + lastNSize + 1)) +
        onLeft +
        msg +
        onRight +
        "\n" +
        post.map(formatLine(_, onLeft, onRight)).mkString("\n")
  }

  type F[T] = (Span, T)
  //type F[T] = (Option[Ctx], Span, T)

  //def mapK[K[_]](nat: F ~> K) => F[T] => K[T]
  //def mapF(fn: F[Unit] => F[Unit]) token.setF(fn(token.unit))
  //type K[T] = (Names, Span, T)

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
