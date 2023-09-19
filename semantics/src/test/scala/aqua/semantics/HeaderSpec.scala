package aqua.semantics

import aqua.parser.head.{ExportExpr, FromExpr, HeaderExpr, ModuleExpr}
import aqua.parser.lexer.{Ability, Name}
import aqua.raw.RawContext
import aqua.raw.arrow.{ArrowRaw, FuncRaw}
import aqua.raw.ops.RawTag
import aqua.raw.value.VarRaw
import aqua.semantics.header.{HeaderHandler, HeaderSem}
import aqua.types.{AbilityType, ArrowType, NilType, ProductType, ScalarType}

import cats.data.{Chain, NonEmptyList, NonEmptyMap, Validated}
import cats.free.Cofree
import cats.{Eval, Id, Monoid}
import cats.syntax.applicative.*
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HeaderSpec extends AnyFlatSpec with Matchers with Inside {

  given Monoid[RawContext] = RawContext.implicits(RawContext.blank).rawContextMonoid

  val handler = new HeaderHandler[Id, RawContext]()

  def exportHeader(funcName: String): Cofree[Chain, HeaderExpr[Id]] = {
    val exp: FromExpr.NameOrAbAs[Id] = Left((Name(funcName), None))

    /**
     * aqua TestModule
     * export <funcName>
     */
    Cofree(
      ModuleExpr(
        name = Ability[Id]("TestModule"),
        declareAll = None,
        declareNames = Nil,
        declareCustom = Nil
      ),
      Chain(
        Cofree(
          ExportExpr(NonEmptyList.of(exp)),
          Chain.empty.pure
        )
      ).pure
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

    val result = handler.sem(Map.empty, ast).andThen(_.finCtx(initCtx))

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

    val result = handler.sem(Map.empty, ast).andThen(_.finCtx(initCtx))

    inside(result) { case Validated.Invalid(errors) =>
      atLeast(1, errors.toChain.toList) shouldBe a[HeaderError[Id]]
    }
  }
}
