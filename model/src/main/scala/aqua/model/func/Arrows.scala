package aqua.model.func

import aqua.raw.arrow.{ArgsCall, FuncArrow, FuncRaw}
import cats.data.State
import cats.syntax.traverse.*
import cats.syntax.functor.*
import cats.instances.list.*

trait Arrows[S] extends Scoped[S] {
  self =>
  def save(name: String, arrow: FuncArrow): State[S, Unit]

  final def resolved(arrow: FuncRaw)(implicit e: Exports[S]): State[S, Unit] =
    for {
      exps <- e.exports
      arrs <- arrows
      funcArrow = arrow.capture(arrs, exps)
      _ <- save(arrow.name, funcArrow)
    } yield ()

  final def resolved(arrows: Map[String, FuncArrow]): State[S, Unit] =
    arrows.toList.traverse(save).void

  val arrows: State[S, Map[String, FuncArrow]]

  def pickArrows(names: Set[String]): State[S, Map[String, FuncArrow]] =
    arrows.map(_.view.filterKeys(names).toMap)

  def argsArrows(args: ArgsCall): State[S, Map[String, FuncArrow]] =
    arrows.map(args.arrowArgs)

  def transformS[R](f: R => S, g: (R, S) => R): Arrows[R] = new Arrows[R] {

    override def save(name: String, arrow: FuncArrow): State[R, Unit] =
      self.save(name, arrow).transformS(f, g)

    override val arrows: State[R, Map[String, FuncArrow]] = self.arrows.transformS(f, g)

    override val purge: State[R, R] =
      self.purgeR(f, g)

    override protected def fill(s: R): State[R, Unit] =
      self.fillR(s, f, g)

  }
}

object Arrows {
  def apply[S](implicit arrows: Arrows[S]): Arrows[S] = arrows

  object Simple extends Arrows[Map[String, FuncArrow]] {

    override def save(name: String, arrow: FuncArrow): State[Map[String, FuncArrow], Unit] =
      State.modify(_ + (name -> arrow))

    override val arrows: State[Map[String, FuncArrow], Map[String, FuncArrow]] =
      State.get

    override val purge: State[Map[String, FuncArrow], Map[String, FuncArrow]] =
      for {
        s <- State.get
        _ <- State.set(Map.empty)
      } yield s

    override protected def fill(s: Map[String, FuncArrow]): State[Map[String, FuncArrow], Unit] =
      State.set(s)
  }
}
