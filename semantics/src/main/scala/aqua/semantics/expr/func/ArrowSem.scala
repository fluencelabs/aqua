package aqua.semantics.expr.func

import aqua.model.func.ArrowModel
import aqua.model.func.raw.{FuncOp, FuncOps, ReturnTag, SeqTag}
import aqua.model.{Model, ValueModel}
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
import cats.data.NonEmptyList
import cats.free.Free
import cats.free.Cofree
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.apply.*

class ArrowSem[F[_]](val expr: ArrowExpr[F]) extends AnyVal {

  import expr.arrowTypeExpr

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

  // TODO: rename, it is not only checks return value
  def checkReturnValue[Alg[_]](
    funcArrow: ArrowType,
    retValue: NonEmptyList[ValueModel],
    body: FuncOp
  )(implicit T: TypesAlgebra[F, Alg], V: ValuesAlgebra[F, Alg]): Free[Alg, Model] =
    if (funcArrow.codomain.length != retValue.length)
      Free.pure[Alg, Model](
        Model.error(
          s"Number of return types does not match: ${arrowTypeExpr.res} declared, ${retValue} returned"
        )
      )
    else
      ((funcArrow.codomain.toList zip retValue.toList)
        .foldLeft(Free.pure[Alg, List[ValueModel]](Nil)) { case (f, (t, v)) =>
          //f.flatMap(vs =>
          // TODO: here we don't have Value[F] as it was handled within ReturnSem
          // Can we pass info about the expected return types to semantics (before), and check that types match in ReturnSem?
//            V.valueToModel(v)
//              .flatTap {
//                case Some(vt) => T.ensureTypeMatches(v, t, vt.lastType).void
//                case None => Free.pure[Alg, Unit](())
//              }
//              .map(vs.prependedAll)
          //)
          // TODO: check the types
          f.map(v :: _)
        })
        .map(_.reverse)
        .map(
          ArrowModel(
            funcArrow,
            _,
            // TODO: wrap with local on...via...
            body
          )
        )

  // TODO: handle all kinds of errors very carefully
  def after[Alg[_]](funcArrow: ArrowType, bodyGen: Model)(implicit
    T: TypesAlgebra[F, Alg],
    N: NamesAlgebra[F, Alg],
    V: ValuesAlgebra[F, Alg],
    A: AbilitiesAlgebra[F, Alg]
  ): Free[Alg, Model] =
    (bodyGen match {
      case m: FuncOp if arrowTypeExpr.res.isEmpty =>
        Free.pure(ArrowModel(funcArrow, Nil, m))

      case m @ FuncOp(Cofree(ReturnTag(retValues), _)) =>
        checkReturnValue(funcArrow, retValues, m)

      case m @ FuncOp(Cofree(SeqTag, tail)) =>
        tail.value.toList.lastOption match {
          case Some(Cofree(ReturnTag(retValues), _)) =>
            checkReturnValue(funcArrow, retValues, m)
          case _ =>
            Free.pure[Alg, Model](
              Model.error(
                "Expected last expression to be <- value, ..."
              )
            )
        }

      case m: FuncOp =>
        // TODO: error with pointer on arrow's return types declaration telling that return value is expected
        Free.pure[Alg, Model](
          Model.error(
            "Return type is defined for the arrow, but nothing returned. Use `<- value, ...` as the last expression inside function body."
          )
        )

      case m =>
        Free.pure[Alg, Model](Model.error("Arrow body is not a funcOp, it's " + m))
    }) <* A.endScope() <* N
      .endScope()

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
