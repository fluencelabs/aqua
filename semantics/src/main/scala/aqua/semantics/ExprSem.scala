package aqua.semantics

import aqua.model.Model
import aqua.parser.Expr
import aqua.parser.expr.*
import aqua.parser.expr.func.*
import aqua.semantics.expr.*
import aqua.semantics.expr.func.*
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import cats.Monad

object ExprSem {

  def getProg[F[_], G[_]: Monad](
    expr: Expr[F]
  )(implicit
    A: AbilitiesAlgebra[F, G],
    N: NamesAlgebra[F, G],
    T: TypesAlgebra[F, G]
  ): Prog[G, Model] =
    expr match {
      case expr: AbilityIdExpr[F] => new AbilityIdSem(expr).program[G]
      case expr: AssignmentExpr[F] => new AssignmentSem(expr).program[G]
      case expr: PushToStreamExpr[F] => new PushToStreamSem(expr).program[G]
      case expr: AliasExpr[F] => new AliasSem(expr).program[G]
      case expr: ConstantExpr[F] => new ConstantSem(expr).program[G]
      case expr: DeclareStreamExpr[F] => new DeclareStreamSem(expr).program[G]
      case expr: ArrowTypeExpr[F] => new ArrowTypeSem(expr).program[G]
      case expr: CallArrowExpr[F] => new CallArrowSem(expr).program[G]
      case expr: DataStructExpr[F] => new DataStructSem(expr).program[G]
      case expr: FieldTypeExpr[F] => new FieldTypeSem(expr).program[G]
      case expr: FuncExpr[F] => new FuncSem(expr).program[G]
      case expr: ClosureExpr[F] => new ClosureSem(expr).program[G]
      case expr: ArrowExpr[F] => new ArrowSem(expr).program[G]
      case expr: OnExpr[F] => new OnSem(expr).program[G]
      case expr: ForExpr[F] => new ForSem(expr).program[G]
      case expr: IfExpr[F] => new IfSem(expr).program[G]
      case expr: TryExpr[F] => new TrySem(expr).program[G]
      case expr: CatchExpr[F] => new CatchSem(expr).program[G]
      case expr: ElseOtherwiseExpr[F] => new ElseOtherwiseSem(expr).program[G]
      case expr: ParExpr[F] => new ParSem(expr).program[G]
      case expr: CoExpr[F] => new CoSem(expr).program[G]
      case expr: ReturnExpr[F] => new ReturnSem(expr).program[G]
      case expr: ServiceExpr[F] => new ServiceSem(expr).program[G]
      case expr: RootExpr[F] => new RootSem(expr).program[G]
    }

}
