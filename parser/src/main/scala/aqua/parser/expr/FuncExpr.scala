package aqua.parser.expr

import aqua.parser.lexer.Token._
import aqua.parser.lexer.{Arg, DataTypeToken, Name, Value}
import aqua.parser.lift.LiftParser
import aqua.parser.{Ast, Expr, FuncReturnError, ParserError}
import cats.Comonad
import cats.data.{Validated, ValidatedNec}
import cats.free.Cofree
import cats.parse.Parser

case class FuncExpr[F[_]](
  name: Name[F],
  args: List[Arg[F]],
  ret: Option[DataTypeToken[F]],
  retValue: Option[Value[F]]
) extends Expr[F](FuncExpr, name)

object FuncExpr extends Expr.AndIndented {

  override def validChildren: List[Expr.Lexem] =
    AbilityIdExpr ::
      AssignmentExpr ::
      ReturnExpr ::
      ForExpr ::
      Expr.defer(OnExpr) ::
      CallArrowExpr ::
      IfExpr ::
      TryExpr ::
      ElseOtherwiseExpr ::
      CatchExpr ::
      ParExpr ::
      DeclareStreamExpr ::
      Nil

  override def p[F[_]: LiftParser: Comonad]: Parser[FuncExpr[F]] =
    ((`func` *> ` ` *> Name.p[F])
      ~ (comma0(` *`.with1 *> Arg.p)).between(`(`, `)`)
      ~ (` -> ` *> DataTypeToken.`datatypedef`).?).map { case ((name, args), ret) =>
      FuncExpr(name, args, ret, None)
    }

  override def ast[F[_]: LiftParser: Comonad](): Parser[ValidatedNec[ParserError[F], Ast.Tree[F]]] =
    super
      .ast()
      .map(
        _.andThen(tree =>
          tree.head match {
            case funcExpr: FuncExpr[F] =>
              funcExpr.ret match {
                case Some(ret) =>
                  tree.tail.value.lastOption.map(_.head) match {
                    case Some(re: ReturnExpr[F]) =>
                      Validated
                        .validNec(Cofree(funcExpr.copy(retValue = Some(re.value)), tree.tail))

                    case _ =>
                      Validated.invalidNec(
                        FuncReturnError[F](
                          ret.unit,
                          "Return type is defined for function, but nothing returned. Use `<- value` as the last expression inside function body."
                        )
                      )
                  }

                case None =>
                  tree.tail.value.lastOption.map(_.head) match {
                    case Some(re: ReturnExpr[F]) =>
                      Validated.invalidNec(
                        FuncReturnError[F](
                          re.value.unit,
                          "Trying to return a value from function that has no return type. Please add return type to function declaration, e.g. `func foo() -> RetType:`"
                        )
                      )
                    case _ =>
                      Validated.validNec(tree)
                  }
              }

            case _ => Validated.validNec(tree)
          }
        )
      )
}
