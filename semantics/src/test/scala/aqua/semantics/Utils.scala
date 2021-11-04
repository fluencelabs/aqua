package aqua.semantics

import aqua.model.{AquaContext, Model}
import aqua.parser.expr.func.ClosureExpr
import aqua.parser.lexer.{Name, Token}
import aqua.parser.lift.Span
import aqua.semantics.expr.func.ClosureSem
import aqua.semantics.rules.ReportError
import aqua.semantics.rules.abilities.{AbilitiesInterpreter, AbilitiesState}
import aqua.semantics.rules.names.{NamesInterpreter, NamesState}
import aqua.semantics.rules.types.{TypesInterpreter, TypesState}
import aqua.types.*
import cats.data.State
import cats.{~>, Id}
import monocle.Lens
import monocle.macros.GenLens
import monocle.syntax.all.*

object Utils {

  implicit val re: ReportError[Id, CompilerState[Id]] =
    (st: CompilerState[Id], token: Token[Id], hint: String) =>
      st.focus(_.errors).modify(_.append(RulesViolated(token, hint)))

  implicit val ns: Lens[CompilerState[Id], NamesState[Id]] = GenLens[CompilerState[Id]](_.names)

  implicit val as: Lens[CompilerState[Id], AbilitiesState[Id]] =
    GenLens[CompilerState[Id]](_.abilities)
  implicit val ts: Lens[CompilerState[Id], TypesState[Id]] = GenLens[CompilerState[Id]](_.types)

  implicit val alg: NamesInterpreter[Id, CompilerState[Id]] =
    new NamesInterpreter[Id, CompilerState[Id]]

  implicit val typesInterpreter: TypesInterpreter[Id, CompilerState[Id]] =
    new TypesInterpreter[Id, CompilerState[Id]]

  implicit val abilitiesInterpreter: AbilitiesInterpreter[Id, CompilerState[Id]] =
    new AbilitiesInterpreter[Id, CompilerState[Id]]

  def spanToId: Span.S ~> Id = new (Span.S ~> Id) {

    override def apply[A](span: Span.S[A]): Id[A] = {
      span._2
    }
  }

  def getModel(prog: Prog[State[CompilerState[cats.Id], *], Model]): Model = {
    prog.apply(emptyS).run(blankCS).value._2
  }

  def getState(
    startState: Model
  )(prog: Prog[State[CompilerState[cats.Id], *], Model]): CompilerState[Id] = {
    prog.apply(State.pure[CompilerState[Id], Model](startState)).run(blankCS).value._1
  }

  def getModel(startState: Model)(prog: Prog[State[CompilerState[cats.Id], *], Model]): Model = {
    prog.apply(State.pure[CompilerState[Id], Model](startState)).run(blankCS).value._2
  }

  def blankCS: CompilerState[Id] = {
    CompilerState.init[Id](AquaContext.blank)
  }

  def labelled(label: String, `type`: Type, tail: ProductType = NilType): LabelledConsType = {
    LabelledConsType(label, `type`, tail)
  }

  def productType(`type`: Type, tail: ProductType = NilType): UnlabelledConsType = {
    UnlabelledConsType(`type`, tail)
  }

  def blank: State[CompilerState[Id], CompilerState[Id]] =
    State.pure[CompilerState[Id], CompilerState[Id]](blankCS)
  def emptyS[F]: State[F, Model] = State.pure[F, Model](Model.empty("empty"))
}
