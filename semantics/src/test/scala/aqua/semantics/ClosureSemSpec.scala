package aqua.semantics

import aqua.model.func.ArrowModel
import aqua.model.func.raw.{EmptyTag, FuncOp}
import aqua.model.{AquaContext, EmptyModel, Model}
import aqua.parser.expr.func.ClosureExpr
import aqua.parser.lexer.{Name, Token}
import aqua.semantics.expr.func.ClosureSem
import aqua.semantics.rules.ReportError
import aqua.semantics.rules.names.{NamesInterpreter, NamesState}
import aqua.types.{ArrowType, ProductType}
import cats.data.*
import cats.data.State.*
import cats.instances.all.*
import cats.syntax.all.*
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.bifunctor.*
import cats.syntax.comonad.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.monad.*
import cats.syntax.semigroup.*
import cats.{Id, Monad, catsInstancesForId}
import monocle.Lens
import monocle.macros.GenLens
import monocle.syntax.all.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class ClosureSemSpec extends AnyFlatSpec with Matchers {

  import Utils.*

  val program: Prog[State[CompilerState[cats.Id], *], Model] = {
    import CompilerState.*
    val expr = ClosureExpr(Name[Id]("closure"))
    val sem = new ClosureSem[Id](expr)

    sem.program[State[CompilerState[Id], *]]
  }

  "sem" should "create right model" in {

    val at = ArrowModel(ArrowType(ProductType(Nil), ProductType(Nil)), Nil, FuncOp.leaf(EmptyTag))
    val model = getModel(program.wrap(blank, (_, _) => State.pure(at)))
    model shouldBe(at)
  }

  "sem" should "create an error" in {
    val model = getModel(program)
    model shouldBe(EmptyModel("Closure must continue with an arrow definition"))
  }
}
