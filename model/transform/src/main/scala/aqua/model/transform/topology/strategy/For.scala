package aqua.model.transform.topology.strategy

import aqua.model.transform.topology.Topology
import aqua.model.{NextModel, OnModel}

import cats.Eval
import cats.syntax.apply.*
import cats.data.Chain.*
import cats.syntax.option.*

object For extends Begins {
  override def toString: String = "<for>"

  // Optimization: get all the path inside the For block out of the block, to avoid repeating
  // hops for every For iteration
  override def beginsOn(current: Topology): Eval[List[OnModel]] =
    // Skip `next` child because its `beginsOn` depends on `this.beginsOn`, see [bug LNG-149]
    (current.forModel zip firstNotNextChild(current).map(_.beginsOn)).map {
      case (model, childBeginsOn) =>
        // Take path until this for's iterator is used
        childBeginsOn.map(
          _.reverse
            .foldLeft((true, List.empty[OnModel])) {
              case ((true, acc), OnModel(_, r)) if r.exists(_.usesVarNames.contains(model.item)) =>
                (false, acc)
              case ((true, acc @ (OnModel(_, r @ (r0 ==: _)) :: _)), OnModel(p, _))
                  if p.usesVarNames.contains(model.item) =>
                // This is to take the outstanding relay and force moving there
                (false, OnModel(r0, r) :: acc)
              case ((true, acc), on) => (true, on :: acc)
              case ((false, acc), _) => (false, acc)
            }
            ._2
        )
    } getOrElse super.beginsOn(current)

  /**
   * Find first child that is not `next`
   */
  private def firstNotNextChild(current: Topology): Option[Topology] = for {
    first <- current.firstChild
    notNext <- first.cursor.op match {
      case _: NextModel => first.nextSibling
      case _ => first.some
    }
  } yield notNext
}
