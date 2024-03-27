package aqua.model.inline.tag

import aqua.errors.Errors.internalError
import aqua.helpers.syntax.reader.*
import aqua.model.*
import aqua.model.ValueModel
import aqua.model.inline.Inline.parDesugarPrefixOpt
import aqua.model.inline.RawValueInliner.valueToModel
import aqua.model.inline.TagInliner.TagInlined
import aqua.model.inline.TagInliner.flat
import aqua.model.inline.state.*
import aqua.model.inline.tag.ForTagInliner.toModel
import aqua.raw.ops.ForTag
import aqua.raw.value.ValueRaw
import aqua.types.CollectionType
import aqua.types.StreamType

import cats.Eval
import cats.data.Reader
import cats.data.{Chain, State}
import cats.syntax.apply.*
import cats.syntax.flatMap.*

final case class ForTagInliner(
  item: String,
  iterable: ValueRaw,
  mode: ForTag.Mode
) {

  def inlined[S: Mangler: Exports: Arrows: Config]: State[S, TagInlined[S]] = for {
    vp <- valueToModel(iterable)
    flattened <- mode match {
      case ForTag.Mode.RecMode => State.pure(vp)
      case _ => flat.tupled(vp)
    }
    (v, p) = flattened
    n <- Mangler[S].findAndForbidName(item)
    elementType = iterable.`type` match {
      case b: CollectionType => b.element
      case _ =>
        internalError(
          s"non-box type variable '$iterable' in 'for' expression."
        )
    }
    _ <- Exports[S].resolved(item, VarModel(n, elementType))
    modeModel = mode match {
      case ForTag.Mode.SeqMode | ForTag.Mode.TryMode => ForModel.Mode.Null
      case ForTag.Mode.ParMode | ForTag.Mode.RecMode => ForModel.Mode.Never
    }
    model = ForModel(n, v, modeModel)
  } yield TagInlined.Around(
    model = toModel(model),
    aroundChildren = identity,
    prefix = p
  )
}

object ForTagInliner {

  def toModel[S: Mangler: Exports: Arrows: Config](model: OpModel)(
    children: State[S, Chain[OpModel.Tree]]
  ): State[S, OpModel.Tree] = Exports[S].subScope(for {
    streamsBefore <- Exports[S].streams
    trees <- children
    streamsAfter <- Exports[S].streams
    streams = streamsAfter.removedAll(streamsBefore.keySet)
    _ <- Exports[S].deleteStreams(streams.keySet)
  } yield build(model, trees, streams))

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
    SeqModel.wrap(streams.toList.foldLeft(Chain.one(model)) { case (acc, (name, st)) =>
      Chain.one(RestrictionModel(name, st).wrap(acc))
    })

  private def build(
    model: OpModel,
    children: Chain[OpModel.Tree],
    streams: Map[String, StreamType]
  ): OpModel.Tree = model.wrap(
    streams.toList.foldLeft(children) { case (acc, (name, st)) =>
      Chain.one(RestrictionModel(name, st).wrap(acc))
    }
  )

}
