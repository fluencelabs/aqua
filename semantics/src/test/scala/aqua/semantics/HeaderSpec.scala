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

import aqua.parser.Ast
import aqua.parser.head.{ExportExpr, FromExpr, HeaderExpr, ModuleExpr}
import aqua.parser.lexer.QName
import aqua.parser.lexer.Token
import aqua.parser.lexer.{Ability, Name}
import aqua.raw.RawContext
import aqua.raw.arrow.{ArrowRaw, FuncRaw}
import aqua.raw.ops.RawTag
import aqua.raw.value.VarRaw
import aqua.semantics.header.{HeaderHandler, HeaderSem}
import aqua.semantics.rules.locations.{DummyLocationsInterpreter, LocationsAlgebra}
import aqua.types.{AbilityType, ArrowType, NilType, ProductType, ScalarType}

import cats.data.{Chain, NonEmptyList, NonEmptyMap, State, Validated}
import cats.free.Cofree
import cats.syntax.applicative.*
import cats.{Eval, Id, Monoid}
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HeaderSpec extends AnyFlatSpec with Matchers with Inside {

  given LocationsAlgebra[Id, State[RawContext, *]] =
    DummyLocationsInterpreter[Id, RawContext]()

  val handler = new HeaderHandler[Id, RawContext]()

  def exportHeader(funcName: String): Ast.Head[Id] = {

    /**
     * aqua TestModule
     * export <funcName>
     */
    Ast.Head(
      Token.lift(()),
      Chain(
        ModuleExpr(
          word = ModuleExpr.Word(ModuleExpr.Word.Kind.Aqua),
          name = QName("TestModule", NonEmptyList.one("TestModule")),
          declares = None
        ),
        ExportExpr(
          Token.lift(()),
          NonEmptyList.of(
            QName.As(
              QName[Id](funcName, NonEmptyList.one(funcName)),
              None
            )
          )
        )
      )
    )
  }

  def funcCtx(funcName: String, arrowType: ArrowType): RawContext =
    RawContext(parts =
      Chain.one(
        (
          RawContext.blank,
          FuncRaw(
            funcName,
            ArrowRaw(arrowType, Nil, RawTag.empty)
          )
        )
      )
    )

  "header handler" should "generate an error on exported function that returns arrow or ability" in {
    val funcName = "funcName"
    val ast = exportHeader(funcName)

    val retArrowType = ArrowType(NilType, NilType)
    val arrowType = ArrowType(NilType, ProductType.apply(retArrowType :: Nil))

    val initCtx = funcCtx(funcName, arrowType)

    val result = handler.sem(Map.empty, ast).andThen(_.fin(initCtx))

    inside(result) { case Validated.Invalid(errors) =>
      atLeast(1, errors.toChain.toList) shouldBe a[HeaderError[Id]]
    }
  }

  it should "generate an error on exported function that accepts an ability" in {
    val funcName = "funcName"
    val ast = exportHeader(funcName)

    val abilityType = AbilityType("Ab", NonEmptyMap.of("field" -> ScalarType.i8))
    val arrowType = ArrowType(ProductType(abilityType :: Nil), NilType)

    val initCtx = funcCtx(funcName, arrowType)

    val result = handler.sem(Map.empty, ast).andThen(_.fin(initCtx))

    inside(result) { case Validated.Invalid(errors) =>
      atLeast(1, errors.toChain.toList) shouldBe a[HeaderError[Id]]
    }
  }
}
