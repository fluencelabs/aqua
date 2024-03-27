package aqua.model.inline.tag

import aqua.model.inline.state.{Arrows, Config, Exports, Mangler}
import aqua.model.inline.TagInliner.TagInlined
import aqua.model.XorModel
import cats.data.State

object TryTagInliner {
  def inlined[S: Mangler: Exports: Arrows: Config]: State[S, TagInlined[S]] = {
    State.pure(TagInlined.Around(model = _.map(XorModel.wrap), aroundChildren = StreamRestrictions.restrictStreamsAround()))
  }
}
