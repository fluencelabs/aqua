package aqua.semantics

import aqua.model.{EmptyModel, Model}
import aqua.parser.expr.func.ArrowExpr
import aqua.parser.lexer.Name
import aqua.semantics.expr.func.ArrowSem
import cats.Id
import cats.data.State
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ArrowSemSpec extends AnyFlatSpec with Matchers with EitherValues {
  import Utils.*

  val program: Prog[State[CompilerState[cats.Id], *], Model] = {
    import CompilerState.*

    val arrow = "(a: string, b: u32) -> u8"

    val expr = ArrowExpr.p.parseAll(arrow).value.mapK(spanToId)
    val sem = new ArrowSem[Id](expr)

    sem.program[State[CompilerState[Id], *]]
  }

  "sem" should "create right model" in {
    val model = getModel(program)
    model shouldBe(EmptyModel("Arrow body is not a funcOp, it's EmptyModel(empty)"))
  }
}
