package aqua.parser.expr

import aqua.parser.lexer.Token.*
import aqua.parser.lexer.{Arg, ArrowTypeToken, DataTypeToken, Name, TypeToken, Value}
import aqua.parser.lift.LiftParser
import aqua.parser.{Ast, Expr, FuncReturnError, ParserError}
import cats.Comonad
import cats.data.{Validated, ValidatedNec}
import cats.free.Cofree
import cats.parse.Parser

case class FuncExpr[F[_]](
  name: Name[F],
  arrowTypeExpr: ArrowTypeToken[F],
  retValue: List[Value[F]]
) extends Expr[F](FuncExpr, name) {
  def ret = arrowTypeExpr.res
}

object FuncExpr extends Expr.AndIndented {

  override def validChildren: List[Expr.Lexem] =
    AbilityIdExpr ::
      AssignmentExpr ::
      PushToStreamExpr ::
      ReturnExpr ::
      ForExpr ::
      Expr.defer(OnExpr) ::
      CallArrowExpr ::
      IfExpr ::
      TryExpr ::
      ElseOtherwiseExpr ::
      CatchExpr ::
      ParExpr ::
      CoExpr ::
      DeclareStreamExpr ::
      Nil

  override def p[F[_]: LiftParser: Comonad]: Parser[FuncExpr[F]] =
    ((`func` *> ` ` *> Name.p[F]) ~ ArrowTypeToken.`arrowWithNames`[F](
      TypeToken.`typedef`[F]
    )).map { case (name, arrow) =>
      FuncExpr(name, arrow.copy(unit = name.unit), Nil)
    }

  override def ast[F[_]: LiftParser: Comonad](): Parser[ValidatedNec[ParserError[F], Ast.Tree[F]]] =
    super
      .ast()
      .map(
        _.andThen(tree =>
          tree.head match {
            case funcExpr: FuncExpr[F] =>
              // Find the return expression which might be the last one in the function body
              val maybeReturn =
                tree.tail.value.lastOption.map(_.head).collect { case re: ReturnExpr[F] =>
                  re
                }
              // Find correspondance between returned values and declared return types
              funcExpr.ret match {
                case Nil =>
                  // Nothing should be returned
                  maybeReturn.fold(Validated.validNec(tree))(re =>
                    // Declared nothing, but smth is returned
                    Validated.invalidNec(
                      FuncReturnError[F](
                        re.token.unit,
                        "Trying to return a value from function that has no return type. Please add return type to function declaration, e.g. `func foo() -> RetType:`"
                      )
                    )
                  )
                case rets =>
                  // Something is expected to be returned
                  maybeReturn.fold(
                    // No values are returned at all, no return expression
                    Validated.invalidNec(
                      FuncReturnError[F](
                        rets.head.unit,
                        "Return type is defined for function, but nothing returned. Use `<- value` as the last expression inside function body."
                      )
                    )
                  ) { re =>
                    // Something is returned, so check that numbers are the same
                    def checkRet(typeDef: List[DataTypeToken[F]], values: List[Value[F]])
                      : ValidatedNec[ParserError[F], Ast.Tree[F]] =
                      (typeDef, values) match {
                        case (Nil, Nil) =>
                          // Everything checked, ok
                          Validated
                            .validNec(Cofree(funcExpr.copy(retValue = re.values.toList), tree.tail))
                        case (_ :: tTail, _ :: vTail) =>
                          // One more element checked, advance
                          checkRet(tTail, vTail)
                        case (t :: _, Nil) =>
                          // No more values, but still have declared types
                          Validated.invalidNec(
                            FuncReturnError[F](
                              t.unit,
                              "Return type is defined for function, but nothing returned. Use `<- value, ...` as the last expression inside function body."
                            )
                          )
                        case (_, v :: _) =>
                          Validated.invalidNec(
                            FuncReturnError[F](
                              v.unit,
                              "Return type is not defined for function, but something is returned."
                            )
                          )
                      }
                    checkRet(rets, re.values.toList)
                  }

              }

            case _ => Validated.validNec(tree)
          }
        )
      )
}
