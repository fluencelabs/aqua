package aqua.model.inline.tag

import aqua.helpers.syntax.reader.*
import aqua.model.*
import aqua.model.inline.Inline.parDesugarPrefix
import aqua.model.inline.RawValueInliner.{valueListToModel, valueToModel}
import aqua.model.inline.TagInliner.{flat, TagInlined}
import aqua.model.inline.state.*
import aqua.raw.ops.OnTag
import aqua.raw.value.ValueRaw
import cats.data.{Chain, State}
import cats.syntax.bifunctor.*
import cats.syntax.traverse.*

final case class OnTagInliner(
  peerId: ValueRaw,
  via: Chain[ValueRaw],
  strategy: Option[OnTag.ReturnStrategy]
) {
  import OnTagInliner.*

  def inlined[S: Mangler: Exports: Arrows: Config]: State[S, TagInlined[S]] =
    for {
      peerIdDe <- valueToModel(peerId)
      viaDe <- valueListToModel(via.toList)
      viaDeFlattened <- viaDe.traverse(flat.tupled)
      (pid, pif) = peerIdDe
      (viaD, viaF) = viaDeFlattened.unzip.bimap(Chain.fromSeq, _.flatten)
      strat = strategy.map { case OnTag.ReturnStrategy.Relay =>
        OnModel.ReturnStrategy.Relay
      }
      noProp <- Config[S].noErrorPropagation.toState
      model = if (noProp) toModelNoProp else toModel
      modelByChildren = model(pid, viaD, strat)
      stateModel = IfTagInliner.wrapWithRestrictions[S](modelByChildren)
    } yield TagInlined.Around(
      prefix = parDesugarPrefix(viaF.prependedAll(pif)),
      model = stateModel
    )

  private def toModelNoProp(
    pid: ValueModel,
    via: Chain[ValueModel],
    strat: Option[OnModel.ReturnStrategy]
  )(children: Chain[OpModel.Tree]): OpModel.Tree =
    OnModel(pid, via, strat).wrap(children)

  private def toModel(
    pid: ValueModel,
    via: Chain[ValueModel],
    strat: Option[OnModel.ReturnStrategy]
  )(children: Chain[OpModel.Tree]): OpModel.Tree =
    XorModel.wrap(
      OnModel(pid, via, strat).wrap(
        children
      ),
      // This will return to previous topology
      // and propagate error up
      FailModel(ValueModel.error).leaf
    )
}
