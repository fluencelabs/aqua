package aqua.parser.expr.func

import aqua.parser.{ArrowReturnError, Ast, Expr, ParserError}
import aqua.parser.lexer.{ArrowTypeToken, DataTypeToken, TypeToken, Value}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.data.{Validated, ValidatedNec}
import cats.free.Cofree
import cats.parse.Parser
import cats.~>

case class ArrowExpr[F[_]](arrowTypeExpr: ArrowTypeToken[F], retValue: List[Value[F]])
    extends Expr[F](ArrowExpr, arrowTypeExpr) {

  override def mapK[K[_]: Comonad](fk: F ~> K): ArrowExpr[K] =
    copy(arrowTypeExpr.mapK(fk), retValue.map(_.mapK(fk)))

}

object ArrowExpr extends Expr.AndIndented {

  val funcChildren: List[Expr.Lexem] =
    AbilityIdExpr ::
      PushToStreamExpr ::
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
      ClosureExpr ::
      AssignmentExpr ::
      Nil

  override val validChildren: List[Expr.Lexem] =
    ReturnExpr :: funcChildren

  override def p[F[_]: LiftParser: Comonad]: Parser[ArrowExpr[F]] =
    ArrowTypeToken
      .`arrowWithNames`[F](
        TypeToken.`typedef`[F]
      )
      .map(ArrowExpr(_, Nil))

  override def afterTreeParsed[F[_]: Comonad]
    : ValidatedNec[ParserError[F], Ast.Tree[F]] => ValidatedNec[ParserError[F], Ast.Tree[F]] =
    _.andThen(tree =>
      tree.head match {
        case arrowExpr: ArrowExpr[F] =>
          // Find the return expression which might be the last one in the function body
          val maybeReturn =
            tree.tail.value.lastOption.map(_.head).collect { case re: ReturnExpr[F] =>
              re
            }
          // Find correspondance between returned values and declared return types
          arrowExpr.arrowTypeExpr.res match {
            case Nil =>
              // Nothing should be returned
              maybeReturn.fold(Validated.validNec(tree))(re =>
                // Declared nothing, but smth is returned
                Validated.invalidNec(
                  ArrowReturnError[F](
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
                  ArrowReturnError[F](
                    rets.head.unit,
                    "Return type is defined for function, but nothing returned. Use `<- value` as the last expression inside function body."
                  )
                )
              ) { re =>
                // Something is returned, so check that numbers are the same
                def checkRet(
                  typeDef: List[DataTypeToken[F]],
                  values: List[Value[F]]
                ): ValidatedNec[ParserError[F], Ast.Tree[F]] =
                  (typeDef, values) match {
                    case (Nil, Nil) =>
                      // Everything checked, ok
                      Validated
                        .validNec(Cofree(arrowExpr.copy(retValue = re.values.toList), tree.tail))
                    case (_ :: tTail, _ :: vTail) =>
                      // One more element checked, advance
                      checkRet(tTail, vTail)
                    case (t :: _, Nil) =>
                      // No more values, but still have declared types
                      Validated.invalidNec(
                        ArrowReturnError[F](
                          t.unit,
                          "Return type is defined for function, but nothing returned. Use `<- value, ...` as the last expression inside function body."
                        )
                      )
                    case (_, v :: _) =>
                      Validated.invalidNec(
                        ArrowReturnError[F](
                          v.unit,
                          "Return type is not defined for function, but something is returned."
                        )
                      )
                  }
                checkRet(rets, re.values.toList)
              }

          }

        case _ =>
          Validated.validNec(tree)
      }
    )
}
