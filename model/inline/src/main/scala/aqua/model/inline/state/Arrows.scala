package aqua.model.inline.state

import aqua.model.ValueModel
import aqua.model.{ArgsCall, FuncArrow}
import aqua.raw.arrow.FuncRaw
import aqua.types.*

import cats.data.State
import cats.instances.list.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.show.*
import cats.syntax.traverse.*

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
      arrs <- arrows
      capturedVars <- Exports[S].gather(arrow.capturedVars.toSeq)
      capturedArrows = arrs.view.filterKeys(arrow.capturedVars).toMap ++
        Arrows.arrowsByValues(arrs, capturedVars)
      funcArrow = FuncArrow.fromRaw(arrow, capturedArrows, capturedVars, topology)
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

  /**
   * Retrieve all arrows that correspond to values
   */
  def arrowsByValues(
    arrows: Map[String, FuncArrow],
    values: Map[String, ValueModel]
  ): Map[String, FuncArrow] = {
    val arrowKeys = arrows.keySet ++ arrows.values.map(_.funcName)
    val varsKeys = values.keySet ++ values.values.collect { case ValueModel.Arrow(vm, _) =>
      vm.name
    }
    val keys = arrowKeys.intersect(varsKeys)

    arrows.filter { case (arrowName, arrow) =>
      keys.contains(arrowName) || keys.contains(arrow.funcName)
    }
  }

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
