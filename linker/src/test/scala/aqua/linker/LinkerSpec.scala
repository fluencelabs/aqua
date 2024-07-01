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

package aqua.linker

import cats.Id
import cats.data.{EitherNec, NonEmptyChain}
import cats.syntax.either.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LinkerSpec extends AnyFlatSpec with Matchers {

  type TP = Map[String, String] => EitherNec[String, String]

  val cycle: NonEmptyChain[String] => String =
    _.toChain.toList.mkString(" -> ")

  "linker" should "resolve dependencies" in {

    val empty = Modules[String, String, TP]()

    val withMod1 = empty.add(
      AquaModule(
        id = "mod1",
        imports = Map("mod2" -> "mod2"),
        dependsOn = Map("mod2" -> "unresolved mod2 in mod1"),
        body = imports => {
          imports
            .get("mod2")
            .toRight("mod2 not found in mod1")
            .toEitherNec
            .map(_ ++ " | mod1")
        }
      ),
      toExport = true
    )
    withMod1.isResolved should be(false)

    Linker.link(withMod1, cycle) should be(
      Left("unresolved mod2 in mod1").toEitherNec
    )

    val withMod2 = withMod1.add(
      AquaModule(
        id = "mod2",
        imports = Map.empty,
        dependsOn = Map.empty,
        body = _ => "mod2".asRight.toEitherNec
      )
    )

    withMod2.isResolved should be(true)

    Linker.link(withMod2, cycle) should be(
      Map(
        "mod1" -> "mod2 | mod1"
      ).asRight.toEitherNec
    )
  }

}
