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
import cats.{Id, Monad, catsInstancesForId}
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
import monocle.Lens
import monocle.macros.GenLens
import monocle.syntax.all.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class ClosureSemSpec extends AnyFlatSpec with Matchers {

  val program = {
    import CompilerState.*
    val expr = ClosureExpr(Name[Id]("closure"))
    val sem = new ClosureSem[Id](expr)
    implicit val re: ReportError[Id, CompilerState[Id]] =
      (st: CompilerState[Id], token: Token[Id], hint: String) =>
        st.focus(_.errors).modify(_.append(RulesViolated(token, hint)))
    implicit val ns: Lens[CompilerState[Id], NamesState[Id]] = GenLens[CompilerState[Id]](_.names)
    implicit val alg: NamesInterpreter[Id, CompilerState[Id]] = new NamesInterpreter[Id, CompilerState[Id]]

    sem.program[State[CompilerState[Id], *]]
  }

  def blankCS: CompilerState[Id] = {
    CompilerState.init[Id](AquaContext.blank)
  }

  def blank = State.pure[CompilerState[Id], CompilerState[Id]](blankCS)
  def emptyS[F] = State.pure[F, Model](Model.empty(""))

  "sem" should "create right model" in {

    val at = ArrowModel(ArrowType(ProductType(Nil), ProductType(Nil)), Nil, FuncOp.leaf(EmptyTag))
    val model = program.wrap(blank, (_, _) => State.pure(at)).apply(emptyS).run(blankCS).value._2
    model shouldBe(at)
  }

  "sem" should "create an error" in {

    val model = program.apply(emptyS).run(CompilerState.init[Id](AquaContext.blank)).value._2
    model shouldBe(EmptyModel("Closure must continue with an arrow definition"))
  }
}
