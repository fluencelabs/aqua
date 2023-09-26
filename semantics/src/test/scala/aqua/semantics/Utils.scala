package aqua.semantics

import aqua.parser.expr.func.ClosureExpr
import aqua.parser.lexer.{Name, Token}
import aqua.parser.lift.Span
import aqua.raw.{Raw, RawContext}
import aqua.semantics.expr.func.ClosureSem
import aqua.semantics.rules.abilities.{AbilitiesAlgebra, AbilitiesInterpreter, AbilitiesState}
import aqua.semantics.rules.locations.{DummyLocationsInterpreter, LocationsAlgebra}
import aqua.semantics.rules.names.{NamesAlgebra, NamesInterpreter, NamesState}
import aqua.semantics.rules.types.{TypesAlgebra, TypesInterpreter, TypesState}
import aqua.semantics.rules.mangler.{ManglerAlgebra, ManglerInterpreter}
import aqua.semantics.rules.report.{ReportAlgebra, ReportInterpreter}
import aqua.types.*

import cats.data.State
import cats.{~>, Id}
import monocle.Lens
import monocle.macros.GenLens
import monocle.syntax.all.*

object Utils {

  given ReportAlgebra[Id, State[CompilerState[Id], *]] =
    new ReportInterpreter[Id, CompilerState[Id]]

  given ManglerAlgebra[State[CompilerState[Id], *]] =
    new ManglerInterpreter[CompilerState[Id]]

  given LocationsAlgebra[Id, State[CompilerState[Id], *]] =
    new DummyLocationsInterpreter[Id, CompilerState[Id]]()

  given NamesAlgebra[Id, State[CompilerState[Id], *]] =
    new NamesInterpreter[Id, CompilerState[Id]]

  given TypesAlgebra[Id, State[CompilerState[Id], *]] =
    new TypesInterpreter[Id, CompilerState[Id]]

  given AbilitiesAlgebra[Id, State[CompilerState[Id], *]] =
    new AbilitiesInterpreter[Id, CompilerState[Id]]

  def spanToId: Span.S ~> Id = new (Span.S ~> Id) {

    override def apply[A](span: Span.S[A]): Id[A] = {
      span._2
    }
  }

  def getModel(prog: Prog[State[CompilerState[cats.Id], *], Raw]): Raw = {
    prog.apply(emptyS).run(blankCS).value._2
  }

  def getState(
    startState: Raw
  )(prog: Prog[State[CompilerState[cats.Id], *], Raw]): CompilerState[Id] = {
    prog.apply(State.pure[CompilerState[Id], Raw](startState)).run(blankCS).value._1
  }

  def getModel(startState: Raw)(prog: Prog[State[CompilerState[cats.Id], *], Raw]): Raw = {
    prog.apply(State.pure[CompilerState[Id], Raw](startState)).run(blankCS).value._2
  }

  def blankCS: CompilerState[Id] = {
    CompilerState.init[Id](RawContext.blank)
  }

  def labelled(label: String, `type`: Type, tail: ProductType = NilType): LabeledConsType = {
    LabeledConsType(label, `type`, tail)
  }

  def productType(`type`: Type, tail: ProductType = NilType): UnlabeledConsType = {
    UnlabeledConsType(`type`, tail)
  }

  def blank: State[CompilerState[Id], CompilerState[Id]] =
    State.pure[CompilerState[Id], CompilerState[Id]](blankCS)

  def emptyS[F]: State[F, Raw] = State.pure[F, Raw](Raw.empty("empty"))
}
