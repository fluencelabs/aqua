package aqua.semantics.rules

import aqua.parser.lexer.Token
import cats.data.State
import monocle.Lens
import cats.syntax.functor.*

case class StackInterpreter[S[_], X, St, Fr](stackLens: Lens[St, List[Fr]])(implicit
  lens: Lens[X, St],
  error: ReportError[S, X]
) {
  type SX[A] = State[X, A]

  def getState: SX[St] = State.get.map(lens.get)
  def setState(st: St): SX[Unit] = State.modify(s => lens.replace(st)(s))

  def reportError(t: Token[S], hints: List[String]): SX[Unit] =
    State.modify(error(_, t, hints))

  def report(t: Token[S], hint: String): SX[Unit] =
    State.modify(error(_, t, hint :: Nil))

  def modify(f: St => St): SX[Unit] =
    State.modify(lens.modify(f))

  def mapStackHead[A](ifStackEmpty: SX[A])(f: Fr => (Fr, A)): SX[A] =
    getState.map(stackLens.get).flatMap {
      case h :: tail =>
        val (updated, result) = f(h)
        modify(stackLens.replace(updated :: tail)).as(result)
      case Nil =>
        ifStackEmpty
    }

  def mapStackHeadE[A](
    ifStackEmpty: SX[A]
  )(f: Fr => Either[(Token[S], String, A), (Fr, A)]): SX[A] =
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

  def endScope: SX[Unit] =
    modify(stackLens.modify(_.tail))

  def beginScope(emptyFrame: Fr): SX[Unit] =
    modify(stackLens.modify(emptyFrame :: _))
}
