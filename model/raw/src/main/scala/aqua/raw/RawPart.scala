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

package aqua.raw

import aqua.types.Type

import cats.Monoid
import cats.data.Chain

trait RawPart extends Raw {
  def name: String

  def rawPartType: Type

  def rename(s: String): RawPart

  def addAbilityName(s: String): RawPart
}

object RawPart {

  case class Parts(parts: Chain[RawPart]) extends Raw

  given Monoid[Parts] with {
    override def empty: Parts = Parts(Chain.empty)

    override def combine(x: Parts, y: Parts): Parts =
      Parts(x.parts ++ y.parts)
  }

  def contextPart(raw: Raw): Parts = raw match {
    case cr: Parts => cr
    case _ =>
      Parts(Chain.one(raw).collect { case rp: RawPart =>
        rp
      })
  }

}
