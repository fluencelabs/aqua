package aqua.semantics.expr

import aqua.model.Model
import aqua.model.func.body.{ForTag, FuncOp, NextTag, OpTag, ParTag, SeqTag, XorTag}
import aqua.parser.expr.ForExpr
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.types.{ArrayType, StreamType, Type}
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
      N.beginScope(expr.item) >> V.resolveType(expr.iterable).flatMap[Option[Type]] {
        case Some(at @ ArrayType(t)) =>
          N.define(expr.item, t).as(Option(at))
        case Some(st @ StreamType(t)) =>
          N.define(expr.item, t).as(Option(st))
        case Some(dt: Type) =>
          T.ensureTypeMatches(expr.iterable, ArrayType(dt), dt).as(Option.empty[Type])
        case _ => Free.pure[Alg, Option[Type]](None)
      },
      (stOpt: Option[Type], ops: Model) =>
        N.endScope() as ((stOpt, ops) match {
          case (Some(t), op: FuncOp) =>
            FuncOp.wrap(
              ForTag(expr.item.value, ValuesAlgebra.valueToModel(expr.iterable, t)),
              FuncOp.node(
                expr.mode.map(_._2).fold[OpTag](SeqTag) {
                  case ForExpr.ParMode => ParTag
                  case ForExpr.TryMode => XorTag
                },
                Chain(op, FuncOp.leaf(NextTag(expr.item.value)))
              )
            )
          case _ => Model.error("Wrong body of For expr")
        })
    )
}
