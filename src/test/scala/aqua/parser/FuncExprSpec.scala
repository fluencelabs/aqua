package aqua.parser

import aqua.parser.expr.{AbilityIdExpr, CoalgebraExpr, FuncExpr, OnExpr}
import aqua.parser.lexer.{Ability, IntoField, Literal, Name, VarLambda}
import cats.data.NonEmptyList
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import aqua.parser.lift.LiftParser.Implicits.idLiftParser
import aqua.semantics.LiteralType
import cats.Id

import scala.language.implicitConversions

class FuncExprSpec extends AnyFlatSpec with Matchers with EitherValues {

  implicit def toAb(str: String): Ability[Id] = Ability[Id](str)

  implicit def toName(str: String): Name[Id] = Name[Id](str)

  implicit def toFields(fields: List[String]): List[IntoField[Id]] = fields.map(f => IntoField[Id](f))

  implicit def toVar(name: String, fields: List[String]): VarLambda[Id] = VarLambda[Id](toName(name), toFields(fields))

  private def parseExpr(str: String) =
    CoalgebraExpr.p[Id].parseAll(str).value

  private def parseAbId(str: String) =
    AbilityIdExpr.p[Id].parseAll(str).value

  private def parseOn(str: String) =
    OnExpr.p[Id].parseAll(str).value

  "func calls" should "parse func()" in {
    parseExpr("func()") should be(CoalgebraExpr[Id](None, None, toName("func"), List()))
    parseExpr("Ab.func(arg)") should be(
      CoalgebraExpr[Id](None, Some(toAb("Ab")), Name[Id]("func"), List(VarLambda[Id](toName("arg"))))
    )

    parseExpr("func(arg.doSomething)") should be(
      CoalgebraExpr[Id](None, None, Name[Id]("func"), List(toVar("arg", List("doSomething"))))
    )

    parseExpr("func(arg.doSomething.and.doSomethingElse)") should be(
      CoalgebraExpr[Id](None, None, Name[Id]("func"), List(toVar("arg", List("doSomething", "and", "doSomethingElse"))))
    )

    parseExpr("Ab.func(arg.doSomething.and.doSomethingElse, arg2.someFunc)") should be(
      CoalgebraExpr[Id](
        None,
        Some(toAb("Ab")),
        Name[Id]("func"),
        List(toVar("arg", List("doSomething", "and", "doSomethingElse")), toVar("arg2", List("someFunc")))
      )
    )

    parseExpr("x <- func(arg.doSomething)") should be(
      CoalgebraExpr[Id](
        Some(toName("x")),
        None,
        Name[Id]("func"),
        List(
          toVar("arg", List("doSomething"))
        )
      )
    )
  }

  "abilities" should "be parsed" in {
    parseAbId("Ab a") should be(
      AbilityIdExpr[Id](toAb("Ab"), toVar("a", List()))
    )

    parseAbId("Ab \"a\"") should be(
      AbilityIdExpr[Id](toAb("Ab"), Literal[Id]("\"a\"", LiteralType.string))
    )

    parseAbId("Ab 1") should be(
      AbilityIdExpr[Id](toAb("Ab"), Literal[Id]("1", LiteralType.number))
    )

    parseAbId("Ab a.id") should be(
      AbilityIdExpr[Id](toAb("Ab"), toVar("a", List("id")))
    )
  }

  "on" should "be parsed" in {
    parseOn("on peer") should be(
      OnExpr[Id](toVar("peer", List()))
    )

    parseOn("on peer.id") should be(
      OnExpr[Id](toVar("peer", List("id")))
    )

    parseOn("on peer.id") should be(
      OnExpr[Id](toVar("peer", List("id")))
    )
  }

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
