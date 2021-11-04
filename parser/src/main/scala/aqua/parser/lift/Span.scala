package aqua.parser.lift

import cats.data.NonEmptyList
import cats.parse.{LocationMap, Parser0, Parser as P}
import cats.{Comonad, Eval}

import scala.language.implicitConversions

case class Span(startIndex: Int, endIndex: Int) {

  def focus(locationMap: Eval[LocationMap], ctx: Int): Option[Span.Focus] = {
    val map = locationMap.value
    map.toLineCol(startIndex).flatMap { case (line, column) =>
      map
        .getLine(line)
        .map { l =>
          val pre =
            (Math.max(0, line - ctx) until line).map(i => map.getLine(i).map(i -> _)).toList.flatten
          val linePos = {
            val (l1, l2) = l.splitAt(column)
            val (lc, l3) = l2.splitAt(endIndex - startIndex)
            (line, l1, lc, l3)
          }
          val post =
            ((line + 1) to (line + ctx)).map(i => map.getLine(i).map(i -> _)).toList.flatten
          Span.Focus(
            pre,
            linePos,
            post,
            column
          )
        }
    }
  }
}

object Span {

  case class Focus(
    pre: List[(Int, String)],
    line: (Int, String, String, String),
    post: List[(Int, String)],
    column: Int
  ) {

    private lazy val lastN = post.lastOption.map(_._1).getOrElse(line._1) + 1
    private lazy val lastNSize = lastN.toString.length

    private def formatLine(l: (Int, String), onLeft: String, onRight: String) =
      formatLN(l._1, onLeft, onRight) + l._2

    private def formatLN(ln: Int, onLeft: String, onRight: String) = {
      val s = (ln + 1).toString
      onLeft + s + (" " * (lastNSize - s.length)) + onRight + " "
    }

    def toConsoleStr(
      msgs: List[String],
      onLeft: String,
      onRight: String = Console.RESET
    ): String = {
      val line3Length = line._3.length
      val line3Mult = if (line3Length == 0) 1 else line3Length
      val message = msgs.map(m => (" " * (line._2.length + lastNSize + 1)) + m).mkString("\n")

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
        ("^" * line3Mult) +
        ("=" * line._4.length) +
        onRight +
        "\n" +
        onLeft +
        message +
        onRight +
        "\n" +
        post.map(formatLine(_, onLeft, onRight)).mkString("\n")
    }
  }

  type S[T] = (Span, T)

  implicit object spanComonad extends Comonad[S] {
    override def extract[A](x: S[A]): A = x._2

    override def coflatMap[A, B](fa: S[A])(f: S[A] ⇒ B): S[B] = fa.copy(_2 = f(fa))

    override def map[A, B](fa: S[A])(f: A ⇒ B): S[B] = fa.copy(_2 = f(fa._2))
  }

  implicit class PToSpan[T](p: P[T]) {
    def lift: P[Span.S[T]] = Span.spanLiftParser.lift(p)
  }

  implicit class P0ToSpan[T](p: Parser0[T]) {
    def lift0: Parser0[Span.S[T]] = Span.spanLiftParser.lift0(p)
  }


  implicit object spanLiftParser extends LiftParser[S] {

    override def lift[T](p: P[T]): P[S[T]] =
      (P.index.with1 ~ p ~ P.index).map { case ((s, v), e) ⇒
        (Span(s, e), v)
      }

    override def lift0[T](p0: Parser0[T]): Parser0[(Span, T)] =
      (P.index ~ p0).map { case (i, v) ⇒
        (Span(i, i), v)
      }

    override def wrapErr(e: P.Error): (Span, P.Error) = {
      (Span(e.failedAtOffset, e.failedAtOffset + 1), e)
    }
  }

}
