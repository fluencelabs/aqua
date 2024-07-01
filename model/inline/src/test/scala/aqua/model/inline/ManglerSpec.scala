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

package aqua.model.inline

import aqua.mangler.ManglerState
import aqua.model.inline.state.Mangler
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ManglerSpec extends AnyFlatSpec with Matchers {

  "mangler" should "rename right" in {
    val mangler = Mangler[ManglerState]

    val results = for {
      res <- mangler.findAndForbidNames(Set("first", "second"))
    } yield res

    val res = results.runA(ManglerState()).value
    res shouldBe Map()
  }

  "mangler" should "rename right if already have renamed" in {
    val mangler = Mangler[ManglerState]

    val results = for {
      res1 <- mangler.findAndForbidNames(Set("first", "first-0", "first-1"))
      res2 <- mangler.findAndForbidNames(Set("first"))
      res3 <- mangler.findAndForbidNames(Set("first-0"))
      res4 <- mangler.findAndForbidNames(Set("first-1"))
      res5 <- mangler.findAndForbidNames(Set("first-2"))
    } yield (res1, res2, res3, res4, res5)

    val (r1, r2, r3, r4, r5) = results.runA(ManglerState()).value
    r1 shouldBe Map()
    r2 shouldBe Map("first" -> "first-2")
    r3 shouldBe Map("first-0" -> "first-0-0")
    r4 shouldBe Map("first-1" -> "first-1-0")
    r5 shouldBe Map("first-2" -> "first-2-0")
  }

  "mangler" should "rename multiple times right" in {
    val mangler = Mangler[ManglerState]

    val results = for {
      res <- mangler.findAndForbidNames(Set("first", "second"))
      res2 <- mangler.findAndForbidNames(Set("first", "second"))
      res3 <- mangler.findAndForbidNames(Set("first"))
      res4 <- mangler.findAndForbidNames(Set("first", "second"))
      res5 <- mangler.findAndForbidNames(Set("second"))
    } yield (res, res2, res3, res4, res5)

    val (r1, r2, r3, r4, r5) = results.runA(ManglerState()).value
    r1 shouldBe Map()
    r2 shouldBe Map("first" -> "first-0", "second" -> "second-0")
    r3 shouldBe Map("first" -> "first-1")
    r4 shouldBe Map("first" -> "first-2", "second" -> "second-1")
    r5 shouldBe Map("second" -> "second-2")
  }

  "mangler" should "forbid and rename right" in {
    val mangler = Mangler[ManglerState]

    val results = for {
      _ <- mangler.forbid(Set("first", "second"))
      res1 <- mangler.findAndForbidNames(Set("first", "second"))
      res2 <- mangler.findAndForbidNames(Set("first"))
      _ <- mangler.forbid(Set("first"))
      _ <- mangler.forbid(Set("first", "second"))
      _ <- mangler.forbid(Set("second"))
      res3 <- mangler.findAndForbidNames(Set("second"))
      res4 <- mangler.findAndForbidNames(Set("second", "first"))
    } yield (res1, res2, res3, res4)

    val (r1, r2, r3, r4) = results.runA(ManglerState()).value
    r1 shouldBe Map("first" -> "first-0", "second" -> "second-0")
    r2 shouldBe Map("first" -> "first-1")
    r3 shouldBe Map("second" -> "second-1")
    r4 shouldBe Map("first" -> "first-2", "second" -> "second-2")
  }

}
