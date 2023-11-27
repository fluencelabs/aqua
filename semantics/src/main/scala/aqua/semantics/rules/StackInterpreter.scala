package aqua.semantics.rules

import aqua.parser.lexer.Token

import cats.data.State
import cats.syntax.functor.*
import cats.syntax.applicative.*
import monocle.Lens

case class StackInterpreter[S[_], X, St, Fr](
  stackLens: Lens[St, List[Fr]]
)(using lens: Lens[X, St]) {
  type SX[A] = State[X, A]

  def getState: SX[St] = State.get.map(lens.get)
  def setState(st: St): SX[Unit] = State.modify(s => lens.replace(st)(s))

  def modify(f: St => St): SX[Unit] =
    State.modify(lens.modify(f))

  def mapStackHead[A](ifStackEmpty: A)(f: Fr => (Fr, A)): SX[A] =
    mapStackHeadM(ifStackEmpty.pure)(f.andThen(_.pure))

  def mapStackHead_(f: Fr => Fr): SX[Unit] =
    mapStackHead(())(f.andThen(_ -> ()))

  def mapStackHeadM[A](ifStackEmpty: => SX[A])(f: Fr => SX[(Fr, A)]): SX[A] =
    getState.map(stackLens.get).flatMap {
      case head :: tail =>
        f(head).flatMap { case (updated, result) =>
          modify(stackLens.replace(updated :: tail)).as(result)
        }
      case Nil => ifStackEmpty
    }

  def endScope: SX[Unit] =
    modify(stackLens.modify(_.tail))

  def beginScope(emptyFrame: Fr): SX[Unit] =
    modify(stackLens.modify(emptyFrame :: _))
}
