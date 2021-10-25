package aqua.semantics.expr

import aqua.model.{Model, ValueModel}
import aqua.model.func.raw._
import aqua.parser.expr.ForExpr
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.types.{ArrayType, BoxType}
import cats.data.Chain
import cats.free.Free
import cats.syntax.flatMap._
import cats.syntax.functor._

class ForSem[F[_]](val expr: ForExpr[F]) extends AnyVal {

  def program[Alg[_]](implicit
    V: ValuesAlgebra[F, Alg],
    N: NamesAlgebra[F, Alg],
    T: TypesAlgebra[F, Alg],
    A: AbilitiesAlgebra[F, Alg]
  ): Prog[Alg, Model] =
    Prog
      .around(
        N.beginScope(expr.item) >> V.valueToModel(expr.iterable).flatMap[Option[ValueModel]] {
          case Some(vm) =>
            vm.lastType match {
              case t: BoxType =>
                N.define(expr.item, t.element).as(Option(vm))
              case dt =>
                T.ensureTypeMatches(expr.iterable, ArrayType(dt), dt).as(Option.empty[ValueModel])
            }

          case _ => Free.pure[Alg, Option[ValueModel]](None)
        },
        (stOpt: Option[ValueModel], ops: Model) =>
          N.endScope() as ((stOpt, ops) match {
            case (Some(vm), op: FuncOp) =>
              // Fix: continue execution after fold par immediately, without finding a path out from par branches
              val innerTag = expr.mode.map(_._2).fold[RawTag](SeqTag) {
                case ForExpr.ParMode => ParTag
                case ForExpr.TryMode => XorTag
              }
              val forTag = FuncOp.wrap(
                ForTag(expr.item.value, vm),
                FuncOp.node(
                  innerTag,
                  Chain(op, FuncOp.leaf(NextTag(expr.item.value)))
                )
              )
              if (innerTag == ParTag) FuncOp.node(ParTag, Chain(forTag, FuncOps.empty))
              else forTag
            case _ => Model.error("Wrong body of For expr")
          })
      )
      .abilitiesScope(expr.token)
}
