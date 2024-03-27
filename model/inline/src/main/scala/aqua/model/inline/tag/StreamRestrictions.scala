package aqua.model.inline.tag

import aqua.model.inline.state.{Arrows, Config, Exports, Mangler}
import aqua.model.{OpModel, RestrictionModel}
import aqua.types.StreamType

import cats.data.{Chain, State}

object StreamRestrictions {

  def toModel[S: Mangler: Exports: Arrows: Config](model: OpModel)(
    children: State[S, Chain[OpModel.Tree]]
  ): State[S, OpModel.Tree] = Exports[S].subScope(for {
    streamsBefore <- Exports[S].streams
    trees <- children
    streamsAfter <- Exports[S].streams
    streams = streamsAfter.removedAll(streamsBefore.keySet)
    _ <- Exports[S].deleteStreams(streams.keySet)
  } yield build(model, trees, streams))

  private def build(
    model: OpModel,
    children: Chain[OpModel.Tree],
    streams: Map[String, StreamType]
  ): OpModel.Tree = model.wrap(
    streams.toList.foldLeft(children) { case (acc, (name, st)) =>
      Chain.one(RestrictionModel(name, st).wrap(acc))
    }
  )

  def wrapWithRestrictions[S: Mangler: Exports: Arrows: Config](
    childrenToModel: Chain[OpModel.Tree] => OpModel.Tree
  )(
    children: State[S, Chain[OpModel.Tree]]
  ): State[S, OpModel.Tree] = Exports[S].subScope(for {
    streamsBefore <- Exports[S].streams
    trees <- children
    model = childrenToModel(trees)
    streamsAfter <- Exports[S].streams
    streams = streamsAfter.removedAll(streamsBefore.keySet)
    _ <- Exports[S].deleteStreams(streams.keySet)
  } yield buildChild(model, streams))

  def aroundChild[S: Mangler: Exports: Arrows: Config](
    child: State[S, OpModel.Tree]
  ): State[S, OpModel.Tree] = Exports[S].subScope(for {
    streamsBefore <- Exports[S].streams
    tree <- child
    streamsAfter <- Exports[S].streams
    streams = streamsAfter.removedAll(streamsBefore.keySet)
    _ <- Exports[S].deleteStreams(streams.keySet)
  } yield buildChild(tree, streams))

  private def buildChild(
    model: OpModel.Tree,
    streams: Map[String, StreamType]
  ): OpModel.Tree =
    streams.toList.foldLeft(model) { case (acc, (name, st)) =>
      RestrictionModel(name, st).wrap(acc)
    }
}
