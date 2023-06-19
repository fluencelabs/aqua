package aqua.model.transform.topology.strategy

import aqua.model.transform.topology.{PathFinder, Topology}
import aqua.model.OnModel

import cats.Eval

trait Ends {

  def endsOn(current: Topology): Eval[List[OnModel]] =
    current.beginsOn

  private def childFinally(
    current: Topology,
    child: Topology => Option[Topology]
  ): Eval[List[OnModel]] =
    child(current).map(lc =>
      lc.forceExit.flatMap {
        case true => current.afterOn
        case false => lc.endsOn
      }
    ) getOrElse current.beginsOn

  protected def lastChildFinally(current: Topology): Eval[List[OnModel]] =
    childFinally(current, _.lastChild)

  protected def firstChildFinally(current: Topology): Eval[List[OnModel]] =
    childFinally(current, _.firstChild)
}
