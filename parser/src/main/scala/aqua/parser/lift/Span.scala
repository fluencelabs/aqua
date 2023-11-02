package aqua.parser.lift

import cats.Comonad
import cats.parse.{LocationMap, Parser0, Parser as P}

import scala.language.implicitConversions

case class Span(startIndex: Int, endIndex: Int) {

  /**
   * Focus on the line pointed by the span
   *
   * @param locationMap Locations Map
   * @param ctx how many lines to capture before and after the line
   * @return Span.Focus
   */
  def focus(locationMap: LocationMap, ctx: Int): Option[Span.Focus] =
    for {
      lineCol <- locationMap.toLineCol(startIndex)
      (lineNum, columnNum) = lineCol
      line <- locationMap.getLine(lineNum)
      focused = Span.focus(line, columnNum, endIndex - startIndex)
      pre = Span.getLines(locationMap, lineNum - ctx, lineNum)
      post = Span.getLines(locationMap, lineNum + 1, lineNum + ctx + 1)
    } yield Span.Focus(
      pre,
      focused.numbered(lineNum),
      post,
      columnNum
    )
}

object Span {

  private def getLines(
    locationMap: LocationMap,
    from: Int,
    to: Int
  ): List[NumberedLine[String]] =
    (from until to)
      .map(i =>
        locationMap
          .getLine(i)
          .map(NumberedLine(i, _))
      )
      .toList
      .flatten

  private def focus(
    str: String,
    idx: Int,
    len: Int
  ): FocusedLine = {
    val end = idx + len
    FocusedLine(
      str.slice(0, idx),
      str.slice(idx, end),
      str.slice(end, str.length)
    )
  }

  final case class NumberedLine[T](
    number: Int,
    line: T
  )

  final case class FocusedLine(
    pre: String,
    focus: String,
    post: String
  ) {

    def numbered(n: Int): NumberedLine[FocusedLine] =
      NumberedLine(n, this)
  }

  case class Focus(
    pre: List[NumberedLine[String]],
    focus: NumberedLine[FocusedLine],
    post: List[NumberedLine[String]],
    column: Int
  ) {

    private lazy val lastN = post.lastOption.map(_.number).getOrElse(focus.number) + 1
    private lazy val lastNSize = lastN.toString.length

    private def formatLine(l: NumberedLine[String], onLeft: String, onRight: String) =
      formatLN(l.number, onLeft, onRight) + l.line

    private def formatLN(ln: Int, onLeft: String, onRight: String) = {
      val s = (ln + 1).toString
      onLeft + s + (" " * (lastNSize - s.length)) + onRight + " "
    }

    /**
     * Format the focus for console output
     *
     * @param msgs Messages to display
     * @param onLeft Control sequence to put on the left
     * @param onRight Control sequence to put on the right
     */
    def toConsoleStr(
      msgs: List[String],
      onLeft: String,
      onRight: String = Console.RESET
    ): String = {
      val focusLength = focus.line.focus.length
      val focusMult = if (focusLength == 0) 1 else focusLength
      val message = msgs
        .map(m => (" " * (focus.line.focus.length + lastNSize + 1)) + m)
        .mkString("\n")

      pre.map(formatLine(_, onLeft, onRight)).mkString("\n") +
        "\n" +
        formatLN(focus.number, onLeft, onRight) +
        focus.line.pre +
        onLeft + focus.line.focus + onRight +
        focus.line.post +
        "\n" +
        (" " * (focus.line.pre.length + lastNSize + 1)) +
        onLeft +
        ("^" * focusMult) +
        ("=" * focus.line.post.length) +
        onRight +
        "\n" +
        onLeft + message + onRight +
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
