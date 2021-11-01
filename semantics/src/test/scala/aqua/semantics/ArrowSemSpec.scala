package aqua.semantics

import aqua.model.func.ArrowModel
import aqua.model.func.raw.{FuncOp, FuncOps, ReturnTag}
import aqua.model.{EmptyModel, LiteralModel, Model}
import aqua.parser.expr.func.ArrowExpr
import aqua.parser.lexer.Name
import aqua.semantics.expr.func.ArrowSem
import aqua.types.*
import cats.Id
import cats.data.{NonEmptyList, State}
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ArrowSemSpec extends AnyFlatSpec with Matchers with EitherValues {
  import Utils.*



  def program(arrowStr: String): Prog[State[CompilerState[cats.Id], *], Model] = {
    import CompilerState.*

    val expr = ArrowExpr.p.parseAll(arrowStr).value.mapK(spanToId)
    val sem = new ArrowSem[Id](expr)

    sem.program[State[CompilerState[Id], *]]
  }

  "sem" should "create empty model" in {
    val model = getModel(program("(a: string, b: u32) -> u8"))
    model shouldBe(EmptyModel("Arrow body is not a funcOp, it's EmptyModel(empty)"))
  }

  "sem" should "create error model" in {
    val model = getModel(FuncOps.empty)(program("(a: string, b: u32) -> u8"))
    model shouldBe(EmptyModel("Return type is defined for the arrow, but nothing returned. Use `<- value, ...` as the last expression inside function body."))
  }

  import aqua.types.ScalarType.*

  "arrow without return type" should "create right model" in {
    val model = getModel(FuncOps.empty)(program("(a: string, b: u32)"))
    model shouldBe ArrowModel(ArrowType(labelled("a", string, labelled("b", u32)), NilType), Nil, FuncOps.empty)
  }

  "arrow with return type and correct state" should "create correct model" in {
    val returnValue = LiteralModel("123", string)
    val returnTag = FuncOp.wrap(ReturnTag(NonEmptyList.one(returnValue)), FuncOps.empty)
    val model = getModel(returnTag)(program("(a: string, b: u32) -> string"))

    val arrowType = ArrowType(labelled("a", string, labelled("b", u32)), productType(string))
    val resultModel = ArrowModel(arrowType, returnValue :: Nil, returnTag)
    model shouldBe resultModel
  }
}
