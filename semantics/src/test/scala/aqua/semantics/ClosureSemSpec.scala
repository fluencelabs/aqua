package aqua.semantics

import aqua.parser.expr.func.ClosureExpr
import aqua.parser.lexer.Name
import aqua.raw.Raw
import aqua.raw.arrow.ArrowRaw
import aqua.raw.ops.RawTag
import aqua.semantics.expr.func.ClosureSem
import aqua.types.{ArrowType, ProductType}

import cats.Id
import cats.data.*
import cats.data.State.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ClosureSemSpec extends AnyFlatSpec with Matchers {

  import Utils.{*, given}

  val program: Prog[State[CompilerState[cats.Id], *], Raw] = {
    val expr = ClosureExpr(Name[Id]("closure"), None)
    val sem = new ClosureSem[Id](expr)

    sem.program[State[CompilerState[Id], *]]
  }

  "sem" should "create right model" in {

    val at =
      ArrowRaw(ArrowType(ProductType(Nil), ProductType(Nil)), Nil, RawTag.empty)
    val model = getModel(program.wrap(blank, (_, _) => State.pure(at)))
    model shouldBe (at)
  }

  "sem" should "create an error" in {
    val model = getModel(program)
    model shouldBe (Raw.Empty("Closure must continue with an arrow definition"))
  }
}
