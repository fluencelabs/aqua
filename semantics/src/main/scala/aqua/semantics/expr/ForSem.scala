package aqua.semantics.expr

import aqua.model.Model
import aqua.model.func.body.{ForTag, FuncOp, NextTag, OpTag, ParTag, SeqTag}
import aqua.parser.expr.ForExpr
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.types.{ArrayType, DataType}
import cats.data.Chain
import cats.free.Free
import cats.syntax.flatMap._
import cats.syntax.functor._

class ForSem[F[_]](val expr: ForExpr[F]) extends AnyVal {

  def program[Alg[_]](implicit
    V: ValuesAlgebra[F, Alg],
    N: NamesAlgebra[F, Alg],
    T: TypesAlgebra[F, Alg]
  ): Prog[Alg, Model] =
    Prog.around(
      N.beginScope(expr.item) >> V.resolveType(expr.iterable).flatMap {
        case Some(ArrayType(t)) =>
          N.define(expr.item, t).void
        case Some(dt: DataType) => T.ensureTypeMatches(expr.iterable, ArrayType(dt), dt).void
        case _ => Free.pure[Alg, Unit](())
      },
      (_: Unit, ops: Model) =>
        // TODO streams should escape the scope
        N.endScope() as (ops match {
          case op: FuncOp =>
            FuncOp.wrap(
              ForTag(expr.item.value, ValuesAlgebra.valueToModel(expr.iterable)),
              FuncOp.node(
                expr.par.fold[OpTag](SeqTag)(_ => ParTag),
                Chain(op, FuncOp.leaf(NextTag(expr.item.value)))
              )
            )
          case _ => Model.error("Wrong body of For expr")
        })
    )
}
