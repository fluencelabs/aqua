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

package aqua.parser.lexer

import aqua.parser.lift.LiftParser.given
import aqua.types.ScalarType

import cats.Id
import cats.parse.Parser
import cats.syntax.option.*
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TypeTokenSpec extends AnyFlatSpec with Matchers with EitherValues {

  import aqua.AquaSpec._

  def stToStt(st: ScalarType): ScalarTypeToken[Id] = ScalarTypeToken(st)

  "basic type token" should "parse scalar types" in {
    ScalarType.all.foreach(st =>
      ScalarTypeToken.`scalartypedef`
        .parseAll(st.name)
        .value
        .mapK(spanToId) should be(stToStt(st))
    )
  }

  it should "not parse empty brackets" in {
    ScalarTypeToken.`scalartypedef`
      .parseAll("()")
      .isLeft should be(true)
  }

  "arrow type token" should "parse type def" in {

    def typedef(str: String) =
      ArrowTypeToken.typeDef().parseAll(str).value.mapK(spanToId)

    typedef("(A -> ())") should be(
      ArrowTypeToken[Id]((), List((None, NamedTypeToken[Id]("A"))), Nil)
    )

    typedef("(A -> B)") should be(
      ArrowTypeToken[Id]((), List((None, NamedTypeToken[Id]("A"))), List(NamedTypeToken[Id]("B")))
    )
  }

  it should "parse return def" in {
    def returndef(str: String) =
      ArrowTypeToken.returnDef().parseAll(str).value.map(_.mapK(spanToId))

    returndef("(A -> B), (C -> D)") should be(
      List(
        ArrowTypeToken[Id](
          (),
          (None, NamedTypeToken[Id]("A")) :: Nil,
          List(NamedTypeToken[Id]("B"))
        ),
        ArrowTypeToken[Id](
          (),
          (None, NamedTypeToken[Id]("C")) :: Nil,
          List(NamedTypeToken[Id]("D"))
        )
      )
    )

    returndef("A, (B, C -> D, E), F -> G, H") should be(
      List(
        NamedTypeToken[Id]("A"),
        ArrowTypeToken[Id](
          (),
          (None, NamedTypeToken[Id]("B")) :: (None, NamedTypeToken[Id]("C")) :: Nil,
          List(NamedTypeToken[Id]("D"), NamedTypeToken[Id]("E"))
        ),
        ArrowTypeToken[Id](
          (),
          (None, NamedTypeToken[Id]("F")) :: Nil,
          List(NamedTypeToken[Id]("G"), NamedTypeToken[Id]("H"))
        )
      )
    )
  }

  it should "parse arrow def" in {
    def arrowdef(str: String) =
      ArrowTypeToken
        .`arrowdef`(BasicTypeToken.`compositetypedef`)
        .parseAll(str)
        .value
        .mapK(spanToId)

    def arrowWithNames(str: String) = ArrowTypeToken
      .`arrowWithNames`(BasicTypeToken.`compositetypedef`)
      .parseAll(str)
      .value
      .mapK(spanToId)

    arrowdef("-> B") should be(
      ArrowTypeToken[Id]((), Nil, List(NamedTypeToken[Id]("B")))
    )

    arrowdef("A -> B") should be(
      ArrowTypeToken[Id](
        (),
        (None -> NamedTypeToken[Id]("A")) :: Nil,
        List(NamedTypeToken[Id]("B"))
      )
    )

    arrowdef("A -> B -> C") should be(
      ArrowTypeToken[Id](
        (),
        (None -> NamedTypeToken[Id]("A")) :: Nil,
        List(
          ArrowTypeToken[Id](
            (),
            (None -> NamedTypeToken[Id]("B")) :: Nil,
            List(NamedTypeToken[Id]("C"))
          )
        )
      )
    )

    arrowdef("A -> B, C -> D") should be(
      ArrowTypeToken[Id](
        (),
        (None -> NamedTypeToken[Id]("A")) :: Nil,
        List(
          ArrowTypeToken[Id](
            (),
            (None -> NamedTypeToken[Id]("B")) :: (None -> NamedTypeToken[Id]("C")) :: Nil,
            List(NamedTypeToken[Id]("D"))
          )
        )
      )
    )

    arrowdef("A -> (B -> F), (C -> D, E)") should be(
      ArrowTypeToken[Id](
        (),
        (None -> NamedTypeToken[Id]("A")) :: Nil,
        List(
          ArrowTypeToken[Id](
            (),
            (None -> NamedTypeToken[Id]("B")) :: Nil,
            NamedTypeToken[Id]("F") :: Nil
          ),
          ArrowTypeToken[Id](
            (),
            (None -> NamedTypeToken[Id]("C")) :: Nil,
            NamedTypeToken[Id]("D") :: NamedTypeToken[Id]("E") :: Nil
          )
        )
      )
    )

    arrowWithNames("(a: A) -> B") should be(
      ArrowTypeToken[Id](
        (),
        (Some(Name[Id]("a")) -> NamedTypeToken[Id]("A")) :: Nil,
        List(NamedTypeToken[Id]("B"))
      )
    )

    arrowWithNames("{SomeAb, SecondAb}(a: A) -> B") should be(
      ArrowTypeToken[Id](
        (),
        (
          Some(Name[Id]("a")) -> NamedTypeToken[Id]("A")
        ) :: Nil,
        List(NamedTypeToken[Id]("B")),
        NamedTypeToken[Id]("SomeAb") :: NamedTypeToken[Id]("SecondAb") :: Nil
      )
    )

    arrowdef("u32 -> Boo") should be(
      ArrowTypeToken[Id](
        (),
        (None -> stToStt(ScalarType.u32)) :: Nil,
        List(NamedTypeToken[Id]("Boo"))
      )
    )

    TypeToken.`typedef`.parseAll("u32 -> ()").value.mapK(spanToId) should be(
      ArrowTypeToken[Id]((), (None -> stToStt(ScalarType.u32)) :: Nil, Nil)
    )

    arrowdef("A, u32 -> B") should be(
      ArrowTypeToken[Id](
        (),
        (None -> NamedTypeToken[Id]("A")) :: (None -> stToStt(ScalarType.u32)) :: Nil,
        List(NamedTypeToken[Id]("B"))
      )
    )

    arrowdef("[]Absolutely, u32 -> B, C") should be(
      ArrowTypeToken[Id](
        (),
        (Option.empty[Name[Id]] -> ArrayTypeToken[Id]((), NamedTypeToken[Id]("Absolutely"))) ::
          (Option.empty[Name[Id]] -> stToStt(ScalarType.u32)) :: Nil,
        NamedTypeToken[Id]("B") ::
          NamedTypeToken[Id]("C") :: Nil
      )
    )

  }

  "data type token" should "parse nested types" in {
    def typedef(str: String): BasicTypeToken[Id] =
      BasicTypeToken.`compositetypedef`.parseAll(str).value.mapK(spanToId)

    val baseTypes: List[(String, BasicTypeToken[Id])] = List(
      "u32" -> stToStt(ScalarType.u32),
      "string" -> stToStt(ScalarType.string),
      "Named" -> NamedTypeToken[Id]("Named")
    )

    val modifiers: List[(String, BasicTypeToken[Id] => BasicTypeToken[Id])] = List(
      "[]" -> ((t: BasicTypeToken[Id]) => ArrayTypeToken[Id]((), t)),
      "?" -> ((t: BasicTypeToken[Id]) => OptionTypeToken[Id]((), t)),
      "*" -> ((t: BasicTypeToken[Id]) => StreamTypeToken[Id]((), t))
    )

    LazyList
      // Generate all cartesian products of modifiers
      .unfold(modifiers)(prod =>
        (
          prod,
          for {
            m <- modifiers
            (sm, mt) = m
            p <- prod
            (sp, pt) = p
          } yield (sm + sp, mt.compose(pt))
        ).some
      )
      .take(6)
      .foreach { mods =>
        for {
          base <- baseTypes
          (bs, bt) = base
          mod <- mods
          (ms, mt) = mod
          // Apply modifiers to base type
          (st, t) = (ms + bs, mt(bt))
        } typedef(st) should be(t)
      }
  }

}
