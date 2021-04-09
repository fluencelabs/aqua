package aqua.parser.lift

import cats.Comonad
import cats.parse.{Parser => P}

import scala.language.implicitConversions

case class FileSpan(name: String, span: Span) {

  def focus(text: String, ctx: Int): Option[FileSpan.Focus] =
    span.focus(text, ctx).map(FileSpan.Focus(name, ctx, _))
}

object FileSpan {

  case class Focus(name: String, ctx: Int, spanFocus: Span.Focus) {

    def toConsoleStr(msg: String, onLeft: String, onRight: String = Console.RESET): String =
      s"$name:$ctx\n" + spanFocus.toConsoleStr(msg, onLeft, onRight)
  }

  type F[T] = (FileSpan, T)

  implicit object spanComonad extends Comonad[F] {
    override def extract[A](x: F[A]): A = x._2

    override def coflatMap[A, B](fa: F[A])(f: F[A] ⇒ B): F[B] = fa.copy(_2 = f(fa))

    override def map[A, B](fa: F[A])(f: A ⇒ B): F[B] = fa.copy(_2 = f(fa._2))
  }

  def fileSpanLiftParser(name: String): LiftParser[F] = new LiftParser[F] {

    override def lift[T](p: P[T]): P[F[T]] = {
      implicitly[LiftParser[Span.F]].lift(p).map { case (span, value) =>
        (FileSpan(name, span), value)
      }
    }
  }
}
