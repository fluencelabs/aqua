package aqua.parser.ast

import aqua.interim.abilities.AbilitiesAlgebra
import aqua.interim.names.NamesAlgebra
import aqua.interim.scope.PeerIdAlgebra
import aqua.interim.types.{ArrayType, ArrowType, Type, TypesAlgebra}
import aqua.parser.lexer.{Arg, DataTypeToken, Name}
import aqua.parser.lift.LiftParser
import cats.{Applicative, Comonad}
import cats.parse.Parser
import aqua.parser.lexer.Token._
import cats.free.Free
import cats.syntax.functor._
import cats.syntax.flatMap._

import scala.collection.immutable.Queue

case class FuncExpr[F[_]](name: Name[F], args: List[Arg[F]], ret: Option[DataTypeToken[F]]) extends Expr[F] {

  def program[Alg[_]](implicit
    T: TypesAlgebra[Alg],
    N: NamesAlgebra[Alg],
    P: PeerIdAlgebra[Alg],
    A: AbilitiesAlgebra[Alg],
    F: Comonad[F]
  ): Prog[Alg, Unit] =
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
                for {
                  acc <- f
                  t <- T.resolveType(argType)
                  _ <- N.define(argName, t)
                } yield acc.enqueue(t)
            }
            .map(_.toList),
          // Resolve return type
          // TODO handle return VALUE!
          ret.fold(Free.pure[Alg, Option[Type]](None))(T.resolveType(_).map(Some(_)))
        )
        .map(argsAndRes => ArrowType(argsAndRes._1, argsAndRes._2)),
      (funcArrow: ArrowType) =>
        // Erase arguments and internal variables
        A.endScope() >> N.endScope() >> N.define(name, funcArrow)
    )

}

object FuncExpr extends Expr.AndIndented(OnExpr, AbilityIdExpr, CoalgebraExpr, ParExpr) {

  override def p[F[_]: LiftParser: Comonad]: Parser[FuncExpr[F]] =
    ((`func` *> ` ` *> Name.p[F]) ~ comma0(Arg.p)
      .between(`(`, `)`) ~ (`->` *> DataTypeToken.`datatypedef`).? <* ` : \n+`).map {
      case ((name, args), ret) => FuncExpr(name, args, ret)
    }
}
