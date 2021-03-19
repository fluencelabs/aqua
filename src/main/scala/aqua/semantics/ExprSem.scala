package aqua.semantics

import aqua.generator.Gen
import aqua.parser.Expr
import aqua.parser.expr._
import aqua.semantics.algebra.abilities.AbilitiesAlgebra
import aqua.semantics.algebra.names.NamesAlgebra
import aqua.semantics.algebra.scope.PeerIdAlgebra
import aqua.semantics.algebra.types.TypesAlgebra
import aqua.semantics.expr._

object ExprSem {

  def getProg[F[_], G[_]](
    expr: Expr[F]
  )(implicit
    A: AbilitiesAlgebra[F, G],
    N: NamesAlgebra[F, G],
    P: PeerIdAlgebra[F, G],
    T: TypesAlgebra[F, G]
  ): Prog[G, Gen] =
    expr match {
      case expr: AbilityIdExpr[F] => new AbilityIdSem(expr).program[G]
      case expr: AliasExpr[F] => new AliasSem(expr).program[G]
      case expr: ArrowTypeExpr[F] => new ArrowTypeSem(expr).program[G]
      case expr: CoalgebraExpr[F] => new CoalgebraSem(expr).program[G]
      case expr: DataStructExpr[F] => new DataStructSem(expr).program[G]
      case expr: FieldTypeExpr[F] => new FieldTypeSem(expr).program[G]
      case expr: FuncExpr[F] => new FuncSem(expr).program[G]
      case expr: OnExpr[F] => new OnSem(expr).program[G]
      case expr: ParExpr[F] => new ParSem(expr).program[G]
      case expr: ReturnExpr[F] => new ReturnSem(expr).program[G]
      case expr: ServiceExpr[F] => new ServiceSem(expr).program[G]
      case expr: RootExpr[F] => new RootSem(expr).program[G]
    }

}
