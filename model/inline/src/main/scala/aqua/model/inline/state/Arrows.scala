package aqua.model.inline.state

import aqua.model.{ArgsCall, FuncArrow}
import aqua.raw.arrow.FuncRaw

import cats.data.State
import cats.instances.list.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import cats.syntax.show.*

/**
 * State algebra for resolved arrows
 *
 * @tparam S
 *   State
 */
trait Arrows[S] extends Scoped[S] {
  self =>
  def save(name: String, arrow: FuncArrow): State[S, Unit]

  /**
   * Arrow is resolved – save it to the state [[S]]
   *
   * @param arrow resolved arrow
   * @param topology captured topology
   */
  final def resolved(
    arrow: FuncRaw,
    topology: Option[String]
  )(using Exports[S]): State[S, Unit] =
    for {
      exps <- Exports[S].exports
      arrs <- arrows
      //   _ = println(s"Resolved arrow: ${arrow.name}")
      //   _ = println(s"Captured var names: ${arrow.capturedVars}")
      captuedVars = exps.filterKeys(arrow.capturedVars).toMap
      capturedArrows = arrs.filterKeys(arrow.capturedVars).toMap
      //   _ = println(s"Captured vars: ${captuedVars}")
      //   _ = println(s"Captured arrows: ${capturedArrows}")
      funcArrow = FuncArrow.fromRaw(arrow, capturedArrows, captuedVars, topology)
      _ <- save(arrow.name, funcArrow)
    } yield ()

  /**
   * Save arrows to the state [[S]]
   *
   * @param arrows
   *   Resolved arrows, accessible by key name which could differ from arrow's name
   */
  final def resolved(arrows: Map[String, FuncArrow]): State[S, Unit] =
    arrows.toList.traverse(save).void

  /**
   * All arrows available for use in scope
   */
  val arrows: State[S, Map[String, FuncArrow]]

  /**
   * Pick a subset of arrows by names
   *
   * @param names
   *   What arrows should be taken
   */
  def pickArrows(names: Set[String]): State[S, Map[String, FuncArrow]] =
    arrows.map(_.view.filterKeys(names).toMap)

  /**
   * Take arrows selected by the function call arguments
   *
   * @param args
   * @return
   */
  def argsArrows(args: ArgsCall): State[S, Map[String, FuncArrow]] =
    arrows.map(args.arrowArgsMap)

  /**
   * Changes the [[S]] type to [[R]]
   *
   * @param f
   *   Lens getter
   * @param g
   *   Lens setter
   * @tparam R
   *   New state type
   */
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

  // Default implementation with the most straightforward state – just a Map
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
