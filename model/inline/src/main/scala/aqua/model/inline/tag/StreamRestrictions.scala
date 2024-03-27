package aqua.model.inline.tag

import aqua.model.inline.state.{Arrows, Config, Exports, Mangler}
import aqua.model.{OpModel, RestrictionModel}
import aqua.types.StreamType

import cats.data.{Chain, State}

object StreamRestrictions {

  // restrict streams that are generated in a tree
  def restrictStreams[S: Mangler: Exports: Arrows: Config](
    childrenToModel: Chain[OpModel.Tree] => OpModel.Tree
  )(children: State[S, Chain[OpModel.Tree]]): State[S, OpModel.Tree] =
    restrictStreamsAround(children.map(childrenToModel))

  // restrict streams that are generated in a tree
  def restrictStreamsAround[S: Mangler: Exports: Arrows: Config](
    child: State[S, OpModel.Tree]
  ): State[S, OpModel.Tree] = {
    for {
      streamsBefore <- Exports[S].streams
      tree <- child
      streamsAfter <- Exports[S].streams
      streams = streamsAfter.removedAll(streamsBefore.keySet)
      _ <- Exports[S].deleteStreams(streams.keySet)
    } yield streams.toList.foldLeft(tree) { case (acc, (name, st)) =>
      RestrictionModel(name, st).wrap(acc)
    }
  }

}
