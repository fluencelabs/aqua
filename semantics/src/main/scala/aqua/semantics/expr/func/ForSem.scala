package aqua.semantics.expr

import aqua.model.{Model, ValueModel}
import aqua.model.func.raw._
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.types.{ArrayType, BoxType}
import cats.data.Chain
import aqua.parser.expr.func.ForExpr
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.applicative._
import cats.Monad

class ForSem[F[_]](val expr: ForExpr[F]) extends AnyVal {

  def program[Alg[_]: Monad](implicit
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

          case _ => None.pure[Alg]
        },
        (stOpt: Option[ValueModel], ops: Model) =>
          N.endScope() as ((stOpt, ops) match {
            case (Some(vm), op: FuncOp) =>
              FuncOp.wrap(
                ForTag(expr.item.value, vm),
                FuncOp.node(
                  expr.mode.map(_._2).fold[RawTag](SeqTag) {
                    case ForExpr.ParMode => ParTag
                    case ForExpr.TryMode => XorTag
                  },
                  Chain(op, FuncOp.leaf(NextTag(expr.item.value)))
                )
              )
            case _ => Model.error("Wrong body of For expr")
          })
      )
      .abilitiesScope[F](expr.token)
}
