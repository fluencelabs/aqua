package aqua.semantics.expr

import aqua.model.func.raw.{FuncOp, FuncOps}
import aqua.model.func.FuncModel
import aqua.model.{Model, ReturnModel, ValueModel}
import aqua.parser.expr.FuncExpr
import aqua.parser.lexer.Arg
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.types.{ArrowType, ProductType, Type}
import cats.Applicative
import cats.data.Chain
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.apply._
import cats.syntax.applicative._
import cats.syntax.monad._
import cats.Monad

class FuncSem[F[_]](val expr: FuncExpr[F]) extends AnyVal {
  import expr._

  def before[Alg[_]: Monad](implicit
    T: TypesAlgebra[F, Alg],
    N: NamesAlgebra[F, Alg],
    V: ValuesAlgebra[F, Alg],
    A: AbilitiesAlgebra[F, Alg]
  ): Alg[ArrowType] =
    A.beginScope(name) >> Applicative[Alg]
      .product(
        // Collect argument types, define local variables
        arrowTypeExpr.args
          .foldLeft(
            // Begin scope -- for mangling
            N.beginScope(name).as[Chain[(String, Type)]](Chain.empty)
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
                    acc.pure[Alg]
                }
              )
            // Unnamed argument
            case (f, _) => f
          }
          .map(_.toList),
        // Resolve return type
        ret.foldLeft[Alg[List[Type]]](Nil.pure[Alg])((f, t) =>
          f.flatMap(ts => T.resolveType(t).map(ts.prependedAll))
        )
      )
      .map(argsAndRes =>
        ArrowType(ProductType.labelled(argsAndRes._1), ProductType(argsAndRes._2.reverse))
      )

  def generateFuncModel[Alg[_]](funcArrow: ArrowType, retModel: List[ValueModel], body: FuncOp)(
    implicit N: NamesAlgebra[F, Alg]
  ): Alg[Model] = {
    val model = FuncModel(
      name = name.value,
      arrowType = funcArrow,
      ret = retModel,
      body = body
    )

    N.defineArrow(
      name,
      funcArrow,
      isRoot = true
    ) as (model: Model)
  }

  def after[Alg[_]: Monad](funcArrow: ArrowType, bodyGen: Model)(implicit
    T: TypesAlgebra[F, Alg],
    N: NamesAlgebra[F, Alg],
    V: ValuesAlgebra[F, Alg],
    A: AbilitiesAlgebra[F, Alg]
  ): Alg[Model] =
    // Check return value type
    ((funcArrow.codomain.toList zip retValue)
      .foldLeft[Alg[List[ValueModel]]](Nil.pure[Alg]) { case (f, (t, v)) =>
        f.flatMap(vs =>
          V.valueToModel(v)
            .flatTap {
              case Some(vt) => T.ensureTypeMatches(v, t, vt.lastType).void
              case None => ().pure[Alg]
            }
            .map(vs.prependedAll)
        )
      })
      .map(_.reverse)
      .flatMap(retModel =>
        // Erase arguments and internal variables
        A.endScope() >> N.endScope() >> (bodyGen match {
          case body: FuncOp if ret.length == retValue.length =>
            generateFuncModel[Alg](funcArrow, retModel, body)
          case ReturnModel =>
            generateFuncModel[Alg](funcArrow, retModel, FuncOps.empty)
          case m => Model.error("Function body is not a funcOp, it's " + m).pure[Alg]
        })
      )

  def program[Alg[_]: Monad](implicit
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
