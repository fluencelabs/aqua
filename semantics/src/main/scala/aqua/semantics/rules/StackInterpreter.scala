package aqua.semantics.rules

import aqua.parser.lexer.Token
import cats.data.State
import monocle.Lens
import cats.syntax.functor._

case class StackInterpreter[F[_], X, St, Fr](stackLens: Lens[St, List[Fr]])(implicit
  lens: Lens[X, St],
  error: ReportError[F, X]
) {
  type S[A] = State[X, A]

  def getState: S[St] = State.get.map(lens.get)
  def setState(st: St): S[Unit] = State.modify(s => lens.replace(st)(s))

  def report(t: Token[F], hint: String): S[Unit] =
    State.modify(error(_, t, hint))

  def modify(f: St => St): S[Unit] =
    State.modify(lens.modify(f))

  def mapStackHead[A](ifStackEmpty: S[A])(f: Fr => (Fr, A)): S[A] =
    getState.map(stackLens.get).flatMap {
      case h :: tail =>
        val (updated, result) = f(h)
        modify(stackLens.replace(updated :: tail)).as(result)
      case Nil =>
        ifStackEmpty
    }

  def mapStackHeadE[A](
    ifStackEmpty: S[A]
  )(f: Fr => Either[(Token[F], String, A), (Fr, A)]): S[A] =
    getState.map(stackLens.get).flatMap {
      case h :: tail =>
        f(h) match {
          case Right((updated, result)) =>
            modify(stackLens.replace(updated :: tail)).as(result)
          case Left((tkn, hint, result)) =>
            report(tkn, hint).as(result)
        }
      case Nil =>
        ifStackEmpty
    }

  def endScope: S[Unit] =
    modify(stackLens.modify(_.tail))

  def beginScope(emptyFrame: Fr): S[Unit] =
    modify(stackLens.modify(emptyFrame :: _))
}
