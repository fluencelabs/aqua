package aqua.parser.expr

import aqua.parser.Ast.Tree
import aqua.parser.{Expr, Indent}
import aqua.parser.lexer.Token._
import aqua.parser.lexer.{Arg, DataTypeToken, Name, Value}
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser._
import cats.free.Cofree
import cats.parse.Parser
import cats.Comonad
import cats.syntax.functor._

case class FuncExpr[F[_]](
  pub: Option[F[Unit]],
  name: Name[F],
  args: List[Arg[F]],
  ret: Option[DataTypeToken[F]],
  retValue: Option[Value[F]]
) extends Expr[F]

object FuncExpr
    extends Expr.AndIndented(
      OnExpr,
      AbilityIdExpr,
      ReturnExpr,
      CallArrowExpr,
      ParExpr,
      ForExpr,
      IfExpr,
      ElseOtherwiseExpr
    ) {

  override def p[F[_]: LiftParser: Comonad]: Parser[FuncExpr[F]] =
    ((`pub` *> ` `).lift.?.map(_.map(_.void)).with1 ~ (`func` *> ` ` *> Name.p[F]) ~ comma0(Arg.p)
      .between(`(`, `)`) ~ (` -> ` *> DataTypeToken.`datatypedef`).?).map {
      case (((pub, name), args), ret) =>
        FuncExpr(pub, name, args, ret, None)
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

        case _: FuncExpr[F] =>
          tree.tail.value.lastOption.map(_.head) match {
            case Some(_: ReturnExpr[F]) =>
              Parser.failWith(
                "Trying to return a value from function that has no return type. Please add return type to function declaration, e.g. `func foo() -> RetType:`"
              )
            case _ =>
              Parser.pure(tree)
          }

        case _ => Parser.pure(tree)
      }
    }
}
