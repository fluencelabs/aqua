/*
 * Copyright (C) 2024  Fluence DAO
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package aqua.model.inline.tag

import aqua.helpers.syntax.reader.*
import aqua.model.*
import aqua.model.inline.Inline.parDesugarPrefix
import aqua.model.inline.RawValueInliner.{valueListToModel, valueToModel}
import aqua.model.inline.TagInliner.flat
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

  def inlined[S: Mangler: Exports: Arrows: Config]: State[S, OnTagInlined] =
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
    } yield OnTagInlined(
      prefix = parDesugarPrefix(viaF.prependedAll(pif)),
      toModel = model(pid, viaD, strat)
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

object OnTagInliner {

  final case class OnTagInlined(
    prefix: Option[OpModel.Tree],
    toModel: Chain[OpModel.Tree] => OpModel.Tree
  )
}
