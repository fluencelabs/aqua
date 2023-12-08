package aqua.semantics

import aqua.parser.Expr
import aqua.parser.expr.*
import aqua.parser.expr.func.*
import aqua.raw.Raw
import aqua.semantics.expr.*
import aqua.semantics.expr.func.*
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.definitions.DefinitionsAlgebra
import aqua.semantics.rules.locations.LocationsAlgebra
import aqua.semantics.rules.mangler.ManglerAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra

import cats.Monad

object ExprSem {

  def getProg[S[_], G[_]: Monad](
    expr: Expr[S]
  )(using
    A: AbilitiesAlgebra[S, G],
    N: NamesAlgebra[S, G],
    T: TypesAlgebra[S, G],
    V: ValuesAlgebra[S, G],
    D: DefinitionsAlgebra[S, G],
    L: LocationsAlgebra[S, G],
    M: ManglerAlgebra[G]
  ): Prog[G, Raw] =
    expr match {
      case expr: ServiceIdExpr[S] => new ServiceIdSem(expr).program[G]
      case expr: AssignmentExpr[S] => new AssignmentSem(expr).program[G]
      case expr: PushToStreamExpr[S] => new PushToStreamSem(expr).program[G]
      case expr: AliasExpr[S] => new AliasSem(expr).program[G]
      case expr: ConstantExpr[S] => new ConstantSem(expr).program[G]
      case expr: DeclareStreamExpr[S] => new DeclareStreamSem(expr).program[G]
      case expr: ArrowTypeExpr[S] => new ArrowTypeSem(expr).program[G]
      case expr: CallArrowExpr[S] => new CallArrowSem(expr).program[G]
      case expr: DataStructExpr[S] => new DataStructSem(expr).program[G]
      case expr: FieldTypeExpr[S] => new FieldTypeSem(expr).program[G]
      case expr: FuncExpr[S] => new FuncSem(expr).program[G]
      case expr: ClosureExpr[S] => new ClosureSem(expr).program[G]
      case expr: ArrowExpr[S] => new ArrowSem(expr).program[G]
      case expr: OnExpr[S] => new OnSem(expr).program[G]
      case expr: ForExpr[S] => new ForSem(expr).program[G]
      case expr: ForkExpr[S] => new ForkSem(expr).program[G]
      case expr: IfExpr[S] => new IfSem(expr).program[G]
      case expr: TryExpr[S] => new TrySem(expr).program[G]
      case expr: CatchExpr[S] => new CatchSem(expr).program[G]
      case expr: ElseOtherwiseExpr[S] => new ElseOtherwiseSem(expr).program[G]
      case expr: ParExpr[S] => new ParSem(expr).program[G]
      case expr: ParSeqExpr[S] => new ParSeqSem(expr).program[G]
      case expr: CoExpr[S] => new CoSem(expr).program[G]
      case expr: JoinExpr[S] => new JoinSem(expr).program[G]
      case expr: ReturnExpr[S] => new ReturnSem(expr).program[G]
      case expr: ServiceExpr[S] => new ServiceSem(expr).program[G]
      case expr: AbilityExpr[S] => new AbilitySem(expr).program[G]
      case expr: RootExpr[S] => new RootSem(expr).program[G]
    }

}
