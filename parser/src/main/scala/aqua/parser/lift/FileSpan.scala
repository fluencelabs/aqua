package aqua.parser.lift

import cats.parse.{LocationMap, Parser0, Parser => P}
import cats.{Comonad, Eval}

import scala.language.implicitConversions

// TODO: rewrite FileSpan and Span under one trait
// TODO: move FileSpan to another package?
case class FileSpan(name: String, locationMap: Eval[LocationMap], span: Span) {

  def focus(ctx: Int): Option[FileSpan.Focus] =
    span.focus(locationMap, ctx).map(FileSpan.Focus(name, locationMap, ctx, _))
}

object FileSpan {

  case class Focus(name: String, locationMap: Eval[LocationMap], ctx: Int, spanFocus: Span.Focus) {

    def toConsoleStr(msgs: List[String], onLeft: String, onRight: String = Console.RESET): String =
      s"$name:${spanFocus.line._1 + 1}:${spanFocus.column + 1}\n" +
        spanFocus.toConsoleStr(
          msgs,
          onLeft,
          onRight
        )
  }

  type F[T] = (FileSpan, T)

  implicit object spanComonad extends Comonad[F] {
    override def extract[A](x: F[A]): A = x._2

    override def coflatMap[A, B](fa: F[A])(f: F[A] ⇒ B): F[B] = fa.copy(_2 = f(fa))

    override def map[A, B](fa: F[A])(f: A ⇒ B): F[B] = fa.copy(_2 = f(fa._2))
  }

  def fileSpanLiftParser(name: String, source: String): LiftParser[F] = new LiftParser[F] {

    private val memoizedLocationMap = Eval.later(LocationMap(source)).memoize

    override def lift[T](p: P[T]): P[F[T]] = {
      implicitly[LiftParser[Span.F]].lift(p).map { case (span, value) =>
        (FileSpan(name, memoizedLocationMap, span), value)
      }
    }

    override def lift0[T](p0: Parser0[T]): Parser0[(FileSpan, T)] = {
      implicitly[LiftParser[Span.F]].lift0(p0).map { case (span, value) =>
        (FileSpan(name, memoizedLocationMap, span), value)
      }
    }

    override def wrapErr(e: P.Error): (FileSpan, P.Error) = (
      FileSpan(
        name,
        memoizedLocationMap,
        Span(e.failedAtOffset, e.failedAtOffset + 1)
      ),
      e
    )
  }
}
