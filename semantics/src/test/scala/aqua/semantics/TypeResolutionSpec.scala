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

package aqua.semantics

import aqua.parser.lexer.*
import aqua.semantics.rules.types.TypeResolution.TypeResolutionError
import aqua.semantics.rules.types.{TypeResolution, TypesState}
import aqua.types.*

import cats.Endo
import cats.Id
import cats.SemigroupK
import cats.data.NonEmptyMap
import cats.data.Validated.*
import cats.kernel.Semigroup
import cats.syntax.foldable.*
import cats.syntax.option.*
import cats.syntax.semigroup.*
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TypeResolutionSpec extends AnyFlatSpec with Matchers with Inside {

  given [A, B]: Semigroup[(Endo[A], Endo[B])] with {
    private val algebraA = SemigroupK[Endo].algebra[A]
    private val algebraB = SemigroupK[Endo].algebra[B]

    override def combine(x: (Endo[A], Endo[B]), y: (Endo[A], Endo[B])): (Endo[A], Endo[B]) =
      (algebraA.combine(x._1, y._1), algebraB.combine(x._2, y._2))
  }

  def stt(st: ScalarType): ScalarTypeToken[Id] = ScalarTypeToken(st)

  def ntt(name: String): NamedTypeToken[Id] = NamedTypeToken(name)

  def resolve(
    token: TypeToken[Id],
    types: Map[String, Type]
  ): TypeResolution.Res[Id, Type] =
    TypeResolution.resolveTypeToken(token)(TypesState(strict = types))

  val validCollectionModifiers: LazyList[
    List[(Endo[BasicTypeToken[Id]], DataType => Type)]
  ] = {
    val baseModifiers: List[(Endo[BasicTypeToken[Id]], Endo[DataType])] = List(
      (ArrayTypeToken[Id]((), _)) -> (ArrayType.apply),
      (OptionTypeToken[Id]((), _)) -> (OptionType.apply)
    )

    val streamModifier = (dt: BasicTypeToken[Id]) => StreamTypeToken[Id]((), dt)

    val dataModifiers = LazyList.unfold(baseModifiers) { mods =>
      (
        mods,
        for {
          m <- mods
          b <- baseModifiers
        } yield m combine b
      ).some
    }

    dataModifiers.map { mods =>
      mods.map { case (token, typ) =>
        (token andThen streamModifier) -> (typ andThen StreamType.apply)
      } ++ mods
    }.prepended(List((streamModifier, StreamType.apply))).take(6)
  }

  val structType = StructType("Struct", NonEmptyMap.of("field" -> ScalarType.i8))

  "TypeResolution resolveTypeToken" should "resolve basic types" in {
    val baseTypes = List(
      stt(ScalarType.u32) -> ScalarType.u32,
      stt(ScalarType.string) -> ScalarType.string,
      ntt("Struct") -> structType
    )

    for {
      base <- baseTypes
      (token, expected) = base
    } inside(resolve(token, Map("Struct" -> structType))) {
      case Valid(TypeResolution(result, _)) =>
        result shouldEqual expected
    }
  }

  it should "resolve nested types" in {
    val baseTypes = List(
      stt(ScalarType.u32) -> ScalarType.u32,
      stt(ScalarType.string) -> ScalarType.string,
      ntt("Struct") -> structType
    )

    validCollectionModifiers
      .take(6)
      .toList
      .flatten
      .foreach(modifier =>
        for {
          base <- baseTypes
          (btoken, btype) = base
          (mod, typ) = modifier
        } inside(resolve(mod(btoken), Map("Struct" -> structType))) {
          case Valid(TypeResolution(result, _)) =>
            result shouldEqual typ(btype)
        }
      )
  }

  it should "forbid services and abilities in collections" in {
    val arrow = NonEmptyMap.of("arrow" -> ArrowType(ProductType(Nil), ProductType(Nil)))

    val serviceType = ServiceType("Srv", arrow)
    val abilityType = AbilityType("Abl", arrow)

    val types = List(
      ntt(serviceType.name) -> serviceType,
      ntt(abilityType.name) -> abilityType
    )

    validCollectionModifiers
      .take(6)
      .toList
      .flatten
      .foreach(modifier =>
        for {
          base <- types
          (btoken, btype) = base
          (mod, _) = modifier
        } inside(
          resolve(
            mod(btoken),
            Map(
              serviceType.name -> serviceType,
              abilityType.name -> abilityType
            )
          )
        ) { case Invalid(errors) =>
          errors.exists(_.hint.contains("contain")) shouldBe true
        }
      )
  }

  it should "forbid streams inside any collection" in {
    val baseTypes = List(
      stt(ScalarType.u32),
      stt(ScalarType.string),
      ntt("Struct")
    )

    val modifiers = validCollectionModifiers
      .map(_.map { case (token, _) => token })
      .take(3)
      .toList
      .flatten

    for {
      left <- modifiers
      right <- identity[BasicTypeToken[Id]] +: modifiers
      base <- baseTypes
      t = left(StreamTypeToken[Id]((), right(base)))
    } inside(
      resolve(t, Map(structType.name -> structType))
    ) { case Invalid(errors) =>
      errors.exists(_.hint.contains("of type *")) shouldBe true
    }
  }

  it should "forbid stream of streams through alias" in {
    val streamType = StreamType(ScalarType.u32)

    val t = StreamTypeToken[Id]((), ntt("Als"))

    inside(
      resolve(t, Map("Als" -> streamType))
    ) { case Invalid(errors) =>
      errors.exists(_.hint.contains("of type *")) shouldBe true
    }
  }
}
