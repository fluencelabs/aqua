package aqua.ast.expr

import aqua.ast.{Expr, Gen, Prog}
import aqua.ast.algebra.abilities.AbilitiesAlgebra
import aqua.ast.algebra.names.NamesAlgebra
import aqua.ast.algebra.scope.PeerIdAlgebra
import aqua.ast.algebra.types.{ArrowType, Type, TypesAlgebra}
import aqua.parser.lexer.Token._
import aqua.parser.lexer.{Arg, DataTypeToken, Name}
import aqua.parser.lift.LiftParser
import cats.free.Free
import cats.parse.Parser
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{Applicative, Comonad}

import scala.collection.immutable.Queue

case class FuncExpr[F[_]](name: Name[F], args: List[Arg[F]], ret: Option[DataTypeToken[F]]) extends Expr[F] {

  def program[Alg[_]](implicit
    T: TypesAlgebra[F, Alg],
    N: NamesAlgebra[F, Alg],
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
                    case Some(t) =>
                      N.define(argName, t).as(acc.enqueue(t))
                    case None =>
                      Free.pure(acc)
                  }
                )
            }
            .map(_.toList),
          // Resolve return type
          // TODO handle return VALUE!
          ret.fold(Free.pure[Alg, Option[Type]](None))(T.resolveType(_))
        )
        .map(argsAndRes => ArrowType(argsAndRes._1, argsAndRes._2)),
      (funcArrow: ArrowType, bodyGen: Gen) =>
        // Erase arguments and internal variables
        A.endScope() >> N.endScope() >> N.define(name, funcArrow, isRoot = true) as Gen(
          s"func ${name.value}:",
          bodyGen :: Nil
        )
    )

}

object FuncExpr extends Expr.AndIndented(OnExpr, AbilityIdExpr, CoalgebraExpr, ParExpr) {

  override def p[F[_]: LiftParser: Comonad]: Parser[FuncExpr[F]] =
    ((`func` *> ` ` *> Name.p[F]) ~ comma0(Arg.p)
      .between(`(`, `)`) ~ (`->` *> DataTypeToken.`datatypedef`).? <* ` : \n+`).map {
      case ((name, args), ret) => FuncExpr(name, args, ret)
    }
}
