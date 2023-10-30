package aqua.semantics

import aqua.parser.expr.func.{AssignmentExpr, ClosureExpr}
import aqua.parser.lexer.CollectionToken.Mode.ArrayMode
import aqua.parser.lexer.{CollectionToken, Name, VarToken}
import aqua.parser.lift.Span
import aqua.raw.Raw
import aqua.semantics.expr.func.{AssignmentSem, ClosureSem}

import cats.Id
import cats.data.State
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AssignmentSemSpec extends AnyFlatSpec with Matchers with Inside {

  import Utils.{given, *}

  val program: Prog[State[CompilerState[cats.Id], *], Raw] = {
    val expr = ClosureExpr(Name[Id]("closure"), None)
    val sem = new ClosureSem[Id](expr)

    sem.program[State[CompilerState[Id], *]]
  }

  "it" should "throw an error on 'nil'" in {
    val expr = AssignmentExpr(Name[Id]("emptyArr"), VarToken[Id](Name[Id]("nil")))
    val sem = new AssignmentSem[Id](expr)

    val state = getState(sem.program[State[CompilerState[Id], *]])

    inside(state.errors) { errors =>
      atLeast(1, errors.toList) shouldBe a[RulesViolated[Span.S]]
    }
  }

  "it" should "throw an error on empty array" in {
    val expr = AssignmentExpr(Name[Id]("emptyArr"), CollectionToken[Id](ArrayMode, Nil))
    val sem = new AssignmentSem[Id](expr)

    val state = getState(sem.program[State[CompilerState[Id], *]])

    inside(state.errors) { errors =>
      atLeast(1, errors.toList) shouldBe a[RulesViolated[Span.S]]
    }
  }
}
