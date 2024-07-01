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

import aqua.model.inline.state.{Arrows, Config, Exports, Mangler}
import aqua.model.inline.TagInliner.TagInlined
import aqua.model.XorModel
import cats.data.State

object TryTagInliner {
  def inlined[S: Mangler: Exports: Arrows: Config]: State[S, TagInlined[S]] =
    State.pure(TagInlined.Around(model = StreamRestrictions.restrictStreams(XorModel.wrap)))
}
