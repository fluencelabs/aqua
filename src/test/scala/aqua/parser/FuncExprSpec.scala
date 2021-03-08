package aqua.parser

import aqua.interim.types.LiteralType
import aqua.parser.lexer.{Ability, ArrowName, IntoField, Literal, Var, VarLambda}
import cats.data.NonEmptyList
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import aqua.parser.lift.LiftParser.Implicits.idLiftParser
import cats.Id
import shapeless.HNil

import scala.language.implicitConversions

class FuncExprSpec extends AnyFlatSpec with Matchers with EitherValues {

  implicit def strToArrow(str: String): ArrowName[Id] = ArrowName[Id](str)
  implicit def strToAb(str: String): Ability[Id] = Ability[Id](str)
  implicit def strToVar(str: String): Var[Id] = Var[Id](str)

  def parseExpr(str: String) =
    FuncExpr.`funcop`[Id]("").parseAll(str).right.value

  def parseBody(str: String) =
    FuncExpr.body[Id].parseAll(str).right.value

  "func calls" should "parse func()" in {
    parseExpr("func()") should be(FuncCall[Id, HNil]("func", Nil, HNil))
    parseExpr("func(arg)") should be(
      FuncCall[Id, HNil]("func", VarLambda[Id]("arg", Nil) :: Nil, HNil)
    )
    parseExpr("func(arg.doSomeThing)") should be(
      FuncCall[Id, HNil]("func", VarLambda[Id]("arg", IntoField[Id]("doSomeThing") :: Nil) :: Nil, HNil)
    )
    parseExpr("func(arg.doSomeThing, arg2)") should be(
      FuncCall[Id, HNil](
        "func",
        VarLambda[Id]("arg", IntoField[Id]("doSomeThing") :: Nil) :: VarLambda[Id]("arg2", Nil) :: Nil,
        HNil
      )
    )
  }

  "ability calls" should "parse Ab.func()" in {
    parseExpr("Ab.func()") should be(AbilityFuncCall[Id, HNil]("Ab", "func", "Ab.func", Nil, HNil))
    parseExpr("Ab.func(arg)") should be(
      AbilityFuncCall[Id, HNil]("Ab", "func", "Ab.func", VarLambda[Id]("arg", Nil) :: Nil, HNil)
    )
    parseExpr("Ab.func(arg.doSomeThing)") should be(
      AbilityFuncCall[Id, HNil](
        "Ab",
        "func",
        "Ab.func",
        VarLambda[Id]("arg", IntoField[Id]("doSomeThing") :: Nil) :: Nil,
        HNil
      )
    )
    parseExpr("Ab.func(arg.doSomeThing, arg2)") should be(
      AbilityFuncCall[Id, HNil](
        "Ab",
        "func",
        "Ab.func",
        VarLambda[Id]("arg", IntoField[Id]("doSomeThing") :: Nil) :: VarLambda[Id]("arg2", Nil) :: Nil,
        HNil
      )
    )
  }

  "extracting" should "parse x <- func()" in {
    parseExpr("someThing <- func()") should be(
      Extract[Id, HNil]("someThing", FuncCall[Id, HNil]("func", Nil, HNil), HNil)
    )
  }

  "extracting" should "parse x <- Ab.func()" in {
    val fCall = AbilityFuncCall[Id, HNil]("Ab", "func", "Ab.func", Nil, HNil)
    parseExpr("x <- Ab.func()") should be(Extract[Id, HNil]("x", fCall, HNil))
  }

  // TODO test with literals
  "ability resolve" should "parse id getter" in {
    parseExpr("Ab x") should be(AbilityId[Id, HNil]("Ab", VarLambda[Id]("x", Nil), HNil))
    parseExpr("Ab x.id") should be(
      AbilityId[Id, HNil]("Ab", VarLambda[Id]("x", IntoField[Id]("id") :: Nil), HNil)
    )
  }

  "on" should "parse startOn" in {
    FuncExpr.startOn.parseAll("on peer: \n").right.value should be(VarLambda[Id]("peer", Nil))
    FuncExpr.startOn.parseAll("on peer.id:\n").right.value should be(VarLambda[Id]("peer", IntoField[Id]("id") :: Nil))
  }

  "on" should "parse on x: y" in {
    val fCall = AbilityFuncCall[Id, HNil]("Ab", "func", "Ab.func", Nil, HNil)
    val extr = Extract[Id, HNil]("x", fCall, HNil)
    val resl = AbilityId[Id, HNil]("Peer", Literal[Id]("\"some id\"", LiteralType.string), HNil)
    val call = FuncCall[Id, HNil]("call", Literal[Id]("true", LiteralType.bool) :: Nil, HNil)

    val script = """on peer.id:
                   | x <- Ab.func()
                   | Peer "some id"
                   | call(true)""".stripMargin

    parseExpr(script) should be(
      On[Id, HNil](VarLambda[Id]("peer", IntoField[Id]("id") :: Nil), NonEmptyList.of(extr, resl, call), HNil)
    )
  }

  "par" should "parse" in {
    parseExpr("par func()") should be(
      Par[Id, HNil]((), FuncCall[Id, HNil]("func", Nil, HNil), HNil)
    )

    val script = """par on peer.id:
                   |
                   | x <- Ab.func()
                   | call(smth)""".stripMargin

    parseExpr(script)
  }

  "body" should "parse several instructions in different orders" in {
    parseBody(""" x <- func()""") should be(
      NonEmptyList.of(Extract[Id, HNil]("x", FuncCall[Id, HNil]("func", Nil, HNil), HNil))
    )
    parseBody(""" x <- func()
                | Peer 3""".stripMargin) should be(
      NonEmptyList.of(
        Extract[Id, HNil]("x", FuncCall[Id, HNil]("func", Nil, HNil), HNil),
        AbilityId[Id, HNil]("Peer", Literal[Id]("3", LiteralType.number), HNil)
      )
    )
    parseBody(""" x <- func()
                | on x:
                |   Peer 3""".stripMargin) should be(
      NonEmptyList.of(
        Extract[Id, HNil]("x", FuncCall[Id, HNil]("func", Nil, HNil), HNil),
        On[Id, HNil](
          VarLambda[Id]("x"),
          NonEmptyList.of(AbilityId[Id, HNil]("Peer", Literal[Id]("3", LiteralType.number), HNil)),
          HNil
        )
      )
    )
    parseBody(""" on x:
                |   Peer 3""".stripMargin) should be(
      NonEmptyList.of(
        On[Id, HNil](
          VarLambda[Id]("x"),
          NonEmptyList.of(AbilityId[Id, HNil]("Peer", Literal[Id]("3", LiteralType.number), HNil)),
          HNil
        )
      )
    )
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

  /*

  On(VarLambda(peer,Some(id)),NonEmptyList(Extract(x,AbilityFuncCall(Ab,FuncCall(func,List()))), AbilityId(Peer,Literal("some id" call(true),List(BasicType(string)))))) was not equal to
  On(VarLambda(peer,Some(id)),NonEmptyList(Extract(x,AbilityFuncCall(Ab,FuncCall(func,List()))), AbilityId(Peer,Literal("some id",List(BasicType(string)))), FuncCall(call,List(Literal(true,List(BasicType(bool)))))))

   */
}
