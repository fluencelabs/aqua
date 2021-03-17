package aqua.ast.expr

import aqua.ast.Ast.Tree
import aqua.ast.algebra.ValuesAlgebra
import aqua.ast.{Expr, Indent, Prog}
import aqua.ast.algebra.abilities.AbilitiesAlgebra
import aqua.ast.algebra.names.NamesAlgebra
import aqua.ast.algebra.scope.PeerIdAlgebra
import aqua.ast.algebra.types.{ArrowType, Type, TypesAlgebra}
import aqua.ast.gen.{AirGen, ArrowGen, FuncBodyGen, FuncGen, Gen}
import aqua.parser.lexer.Token._
import aqua.parser.lexer.{Arg, DataTypeToken, Name, Value}
import aqua.parser.lift.LiftParser
import cats.free.{Cofree, Free}
import cats.parse.Parser
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{Applicative, Comonad}

import scala.collection.immutable.Queue

case class FuncExpr[F[_]](name: Name[F], args: List[Arg[F]], ret: Option[DataTypeToken[F]], retValue: Option[Value[F]])
    extends Expr[F] {

  def program[Alg[_]](implicit
    T: TypesAlgebra[F, Alg],
    N: NamesAlgebra[F, Alg],
    V: ValuesAlgebra[F, Alg],
    P: PeerIdAlgebra[F, Alg],
    A: AbilitiesAlgebra[F, Alg]
  ): Prog[Alg, Gen] =
    Prog.around(
      A.beginScope(name) >> Applicative[Free[Alg, *]]
        .product(
          // Collect argument types, define local variables
          args
            .foldLeft(
              // Begin scope -- for mangling
              N.beginScope(name).as[Queue[Type]](Queue.empty)
            ) {
              case (f, Arg(argName, argType)) =>
                // Resolve arg type, remember it
                f.flatMap(acc =>
                  T.resolveType(argType).flatMap {
                    case Some(t: ArrowType) =>
                      N.defineArrow(argName, ArrowGen.arg(t), isRoot = false).as(acc.enqueue(t))
                    case Some(t) =>
                      N.define(argName, t).as(acc.enqueue(t))
                    case None =>
                      Free.pure(acc)
                  }
                )
            }
            .map(_.toList),
          // Resolve return type
          ret.fold(Free.pure[Alg, Option[Type]](None))(T.resolveType(_))
        )
        .map(argsAndRes => ArrowType(argsAndRes._1, argsAndRes._2)),
      (funcArrow: ArrowType, bodyGen: Gen) =>
        // Check return value type
        ((funcArrow.res, retValue) match {
          case (Some(t), Some(v)) =>
            V.resolveType(v).flatMap {
              case Some(vt) => T.ensureTypeMatches(v, t, vt).void
              case None => Free.pure[Alg, Unit](())
            }
          case _ =>
            Free.pure[Alg, Unit](())
        })
        // Erase arguments and internal variables
          >> A.endScope() >> N.endScope() >> (bodyGen match {
          case bg: AirGen if ret.isDefined == retValue.isDefined =>
            N.defineArrow(name, ArrowGen.func(funcArrow), isRoot = true) as FuncGen(
              name.value, // TODO: handle return value
              FuncBodyGen(bg)
            )
          case _ => Gen.noop.lift
        })
    )

}

object FuncExpr extends Expr.AndIndented(OnExpr, AbilityIdExpr, ReturnExpr, CoalgebraExpr, ParExpr) {

  override def p[F[_]: LiftParser: Comonad]: Parser[FuncExpr[F]] =
    ((`func` *> ` ` *> Name.p[F]) ~ comma0(Arg.p)
      .between(`(`, `)`) ~ (` -> ` *> DataTypeToken.`datatypedef`).? <* ` : \n+`).map {
      case ((name, args), ret) => FuncExpr(name, args, ret, None)
    }

  override def ast[F[_]: LiftParser: Comonad](ps: Indent): Parser[Tree[F]] =
    super.ast(ps).flatMap { tree =>
      tree.head match {
        case funcExpr: FuncExpr[F] if funcExpr.ret.isDefined =>
          tree.tail.value.lastOption.map(_.head) match {
            case Some(re: ReturnExpr[F]) =>
              Parser.pure(
                Cofree(funcExpr.copy(retValue = Some(re.value)), tree.tail)
              )
            case _ =>
              Parser.failWith(
                "Return type is defined for function, but nothing returned. Use `<- value` as the last expression inside function body."
              )
          }

        case _ => Parser.pure(tree)
      }
    }
}
