package aqua.parser

import aqua.parser.expr.{AbilityIdExpr, CoalgebraExpr, FuncExpr, OnExpr}
import aqua.parser.lexer.{Ability, IntoField, Literal, Name, VarLambda}
import cats.data.NonEmptyList
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import aqua.parser.lift.LiftParser.Implicits.idLiftParser
import cats.Id

import scala.language.implicitConversions

class FuncExprSpec extends AnyFlatSpec with Matchers with EitherValues {
  "on" should "parse on x: y" in {
    val script =
      """func a():
        | on peer.id:
        |  x <- Ab.func()
        |  Peer "some id"
        |  call(true)""".stripMargin

    FuncExpr.ast[Id](Indent()).parseAll(script).isRight should be(true)
  }
  "if" should "parse if x == y" in {
    val script =
      """func a():
        | if peer.id == other:
        |  x <- Ab.func()
        |  Peer "some id"
        |  call(true)""".stripMargin

    FuncExpr.ast[Id](Indent()).parseAll(script).isRight should be(true)
  }

  /*
  TODO: xor1
try:
 ...
catch( errn)?:
 ...
(next)
   */
  /*
  TODO: xor2
if a == != b:
 ...
else:
 ...
(next)
   */
  /*
  TODO: ability from lens
   */
  /*
  TODO: fold, fold par, streams, ...

  for x <- $xs:
   */
}
