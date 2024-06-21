/*
 * Copyright (C) 2024  Fluence DAO
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

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
