package aqua.model.inline.tag

import aqua.model.inline.state.{Arrows, Config, Exports, Mangler}
import aqua.model.{OpModel, RestrictionModel}
import aqua.types.StreamType

import cats.data.{Chain, State}
import cats.syntax.traverse.*

object StreamRestrictions {

  // restrict streams that are generated in a tree
  def restrictStreams[S: Mangler: Exports: Arrows: Config](
    childrenToModel: Chain[OpModel.Tree] => OpModel.Tree
  )(children: Chain[State[S, OpModel.Tree]]): State[S, OpModel.Tree] =
    children.traverse(restrictStreamsAround).map(childrenToModel)

  /**
   * Restrict streams that are generated in a tree
   */
  private def restrictStreamsAround[S: Mangler: Exports: Arrows: Config](
    getTree: State[S, OpModel.Tree]
  ): State[S, OpModel.Tree] =
    Exports[S].streamScope(getTree).map { case (tree, streams) =>
      streams.toList.foldLeft(tree) { case (acc, (name, st)) =>
        RestrictionModel(name, st).wrap(acc)
      }
    }
}
