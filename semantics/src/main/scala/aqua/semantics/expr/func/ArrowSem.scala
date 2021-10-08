package aqua.semantics.expr.func

import aqua.model.func.ArrowModel
import aqua.model.func.raw.{FuncOp, FuncOps}
import aqua.model.{Model, ReturnModel, ValueModel}
import aqua.parser.expr.FuncExpr
import aqua.parser.expr.func.ArrowExpr
import aqua.parser.lexer.Arg
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.types.{ArrowType, ProductType, Type}
import cats.Applicative
import cats.data.Chain
import cats.free.Free
import cats.syntax.flatMap.*
import cats.syntax.functor.*

class ArrowSem[F[_]](val expr: ArrowExpr[F]) extends AnyVal {

  import expr.*

  def before[Alg[_]](implicit
    T: TypesAlgebra[F, Alg],
    N: NamesAlgebra[F, Alg],
    V: ValuesAlgebra[F, Alg],
    A: AbilitiesAlgebra[F, Alg]
  ): Free[Alg, ArrowType] =
    A.beginScope(arrowTypeExpr) >> Applicative[Free[Alg, *]]
      .product(
        // Collect argument types, define local variables
        arrowTypeExpr.args
          .foldLeft(
            // Begin scope -- for mangling
            N.beginScope(arrowTypeExpr).as[Chain[(String, Type)]](Chain.empty)
          ) {
            case (f, (Some(argName), argType)) =>
              // Resolve arg type, remember it
              f.flatMap(acc =>
                T.resolveType(argType).flatMap {
                  case Some(t: ArrowType) =>
                    N.defineArrow(argName, t, isRoot = false).as(acc.append(argName.value -> t))
                  case Some(t) =>
                    N.define(argName, t).as(acc.append(argName.value -> t))
                  case None =>
                    Free.pure(acc)
                }
              )
            // Unnamed argument
            case (f, _) => f
          }
          .map(_.toList),
        // Resolve return type
        arrowTypeExpr.res.foldLeft(Free.pure[Alg, List[Type]](Nil))((f, t) =>
          f.flatMap(ts => T.resolveType(t).map(ts.prependedAll))
        )
      )
      .map(argsAndRes =>
        ArrowType(ProductType.labelled(argsAndRes._1), ProductType(argsAndRes._2.reverse))
      )

  def after[Alg[_]](funcArrow: ArrowType, bodyGen: Model)(implicit
    T: TypesAlgebra[F, Alg],
    N: NamesAlgebra[F, Alg],
    V: ValuesAlgebra[F, Alg],
    A: AbilitiesAlgebra[F, Alg]
  ): Free[Alg, Model] =
    // Check return value type
    ((funcArrow.codomain.toList zip retValue)
      .foldLeft(Free.pure[Alg, List[ValueModel]](Nil)) { case (f, (t, v)) =>
        f.flatMap(vs =>
          V.valueToModel(v)
            .flatTap {
              case Some(vt) => T.ensureTypeMatches(v, t, vt.lastType).void
              case None => Free.pure[Alg, Unit](())
            }
            .map(vs.prependedAll)
        )
      })
      .map(_.reverse)
      .flatMap(retModel =>
        // Erase arguments and internal variables
        A.endScope() >> N.endScope() as (bodyGen match {
          case body: FuncOp if arrowTypeExpr.res.length == retValue.length =>
            ArrowModel(funcArrow, retModel, body)
          case ReturnModel =>
            ArrowModel(funcArrow, retModel, FuncOps.empty)
          case m: FuncOp =>
            Model.error(
              s"Number of return types does not match: ${arrowTypeExpr.res} declared, ${retValue} returned"
            )
          case m => Model.error("Arrow body is not a funcOp, it's " + m)
        })
      )

  def program[Alg[_]](implicit
    T: TypesAlgebra[F, Alg],
    N: NamesAlgebra[F, Alg],
    V: ValuesAlgebra[F, Alg],
    A: AbilitiesAlgebra[F, Alg]
  ): Prog[Alg, Model] =
    Prog.around(
      before[Alg],
      after[Alg]
    )

}
