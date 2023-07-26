package aqua.semantics

import aqua.parser.head.{ExportExpr, FromExpr, HeaderExpr}
import aqua.parser.lexer.Name
import aqua.raw.RawContext
import aqua.raw.arrow.{ArrowRaw, FuncRaw}
import aqua.raw.ops.RawTag
import aqua.raw.value.VarRaw
import aqua.semantics.header.{HeaderHandler, HeaderSem}
import aqua.types.{ArrowType, NilType, ProductType}
import cats.data.{Chain, NonEmptyList, Validated}
import cats.free.Cofree
import cats.{Eval, Id, Monoid}
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HeaderSpec extends AnyFlatSpec with Matchers with Inside {

  "header handler" should "generate an error on exported function that returns arrow or ability" in {
    implicit val rc: Monoid[RawContext] = RawContext.implicits(RawContext.blank).rawContextMonoid

    val handler = new HeaderHandler[Id, RawContext]()

    val funcName = "funcName"

    val exp: FromExpr.NameOrAbAs[Id] = Left((Name[Id](funcName), None))
    val ast =
      Cofree[Chain, HeaderExpr[Id]](ExportExpr[Id](NonEmptyList.of(exp)), Eval.now(Chain.empty))

    val retArrowType = ArrowType(NilType, NilType)
    val arrowType = ArrowType(NilType, ProductType.apply(retArrowType :: Nil))

    val initCtx = RawContext(parts =
      Chain.one(
        (
          RawContext.blank,
          FuncRaw(funcName, ArrowRaw(arrowType, VarRaw("", retArrowType) :: Nil, RawTag.empty))
        )
      )
    )

    val result = handler.sem(Map.empty, ast).andThen(_.finCtx(initCtx))
    
    inside(result) {
      case Validated.Invalid(errors) =>
        errors.head shouldBe a [HeaderError[Id]]   
    }
  }
}
