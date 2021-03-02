package aqua.parser

import aqua.parser.lexer.{Ability, ArrowName, BasicType, IntoField, Literal, Var, VarLambda}
import cats.data.NonEmptyList
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import aqua.parser.lift.LiftParser.Implicits.idLiftParser
import cats.Id
import shapeless.HNil

import scala.language.implicitConversions

class FuncOpSpec extends AnyFlatSpec with Matchers with EitherValues {

  implicit def strToArrow(str: String): ArrowName[Id] = ArrowName[Id](str)
  implicit def strToAb(str: String): Ability[Id] = Ability[Id](str)
  implicit def strToVar(str: String): Var[Id] = Var[Id](str)

  "func calls" should "parse func()" in {
    FuncOp.`funcop`.parseAll("func()") should be(Right(FuncCall[Id, HNil]("func", Nil, HNil)))
    FuncOp.`funcop`.parseAll("func(arg)") should be(
      Right(FuncCall[Id, HNil]("func", VarLambda[Id]("arg", Nil) :: Nil, HNil))
    )
    FuncOp.`funcop`.parseAll("func(arg.doSomeThing)") should be(
      Right(FuncCall[Id, HNil]("func", VarLambda[Id]("arg", IntoField[Id]("doSomeThing") :: Nil) :: Nil, HNil))
    )
    FuncOp.`funcop`.parseAll("func(arg.doSomeThing, arg2)") should be(
      Right(
        FuncCall[Id, HNil](
          "func",
          VarLambda[Id]("arg", IntoField[Id]("doSomeThing") :: Nil) :: VarLambda[Id]("arg2", Nil) :: Nil,
          HNil
        )
      )
    )
  }

  "ability calls" should "parse Ab.func()" in {
    FuncOp.`funcop`.parseAll("Ab.func()") should be(Right(AbilityFuncCall[Id, HNil]("Ab", "Ab.func", Nil, HNil)))
    FuncOp.`funcop`.parseAll("Ab.func(arg)") should be(
      Right(AbilityFuncCall[Id, HNil]("Ab", "Ab.func", VarLambda[Id]("arg", Nil) :: Nil, HNil))
    )
    FuncOp.`funcop`.parseAll("Ab.func(arg.doSomeThing)") should be(
      Right(
        AbilityFuncCall[Id, HNil](
          "Ab",
          "Ab.func",
          VarLambda[Id]("arg", IntoField[Id]("doSomeThing") :: Nil) :: Nil,
          HNil
        )
      )
    )
    FuncOp.`funcop`.parseAll("Ab.func(arg.doSomeThing, arg2)") should be(
      Right(
        AbilityFuncCall[Id, HNil](
          "Ab",
          "Ab.func",
          VarLambda[Id]("arg", IntoField[Id]("doSomeThing") :: Nil) :: VarLambda[Id]("arg2", Nil) :: Nil,
          HNil
        )
      )
    )
  }

  "extracting" should "parse x <- func()" in {
    FuncOp.`funcop`.parseAll("someThing <- func()") should be(
      Right(Extract[Id, HNil]("someThing", FuncCall[Id, HNil]("func", Nil, HNil), HNil))
    )
  }

  "extracting" should "parse x <- Ab.func()" in {
    val fCall = AbilityFuncCall[Id, HNil]("Ab", "Ab.func", Nil, HNil)
    FuncOp.`funcop`.parseAll("x <- Ab.func()") should be(Right(Extract[Id, HNil]("x", fCall, HNil)))
  }

  // TODO test with literals
  "ability resolve" should "parse id getter" in {
    FuncOp.`funcop`.parseAll("Ab x") should be(Right(AbilityId[Id, HNil]("Ab", VarLambda[Id]("x", Nil), HNil)))
    FuncOp.`funcop`.parseAll("Ab x.id") should be(
      Right(AbilityId[Id, HNil]("Ab", VarLambda[Id]("x", IntoField[Id]("id") :: Nil), HNil))
    )
  }

  "on" should "parse startOn" in {
    FuncOp.startOn.parseAll("on peer: \n") should be(Right(VarLambda[Id]("peer", Nil)))
    FuncOp.startOn.parseAll("on peer.id:\n") should be(Right(VarLambda[Id]("peer", IntoField[Id]("id") :: Nil)))
  }

  "on" should "parse on x: y" in {
    val fCall = AbilityFuncCall[Id, HNil]("Ab", "Ab.func", Nil, HNil)
    val extr = Extract[Id, HNil]("x", fCall, HNil)
    val resl = AbilityId[Id, HNil]("Peer", Literal[Id]("\"some id\"", BasicType.string), HNil)
    val call = FuncCall[Id, HNil]("call", Literal[Id]("true", BasicType.bool) :: Nil, HNil)

    val script = """on peer.id:
                   | x <- Ab.func()
                   | Peer "some id"
                   | call(true)""".stripMargin

    FuncOp.`funcop`.parseAll(script).right.value should be(
      On[Id, HNil](VarLambda[Id]("peer", IntoField[Id]("id") :: Nil), NonEmptyList.of(extr, resl, call), HNil)
    )
  }

  "par" should "parse" in {
    FuncOp.`funcop`.parseAll("par func()") should be(
      Right(Par[Id, HNil]((), FuncCall[Id, HNil]("func", Nil, HNil), HNil))
    )

    val script = """par on peer.id:
                   |
                   | x <- Ab.func()
                   | call(smth)""".stripMargin

    FuncOp.`funcop`.parseAll(script) should be('right)
  }

  "body" should "parse several instructions in different orders" in {
    FuncOp.body[Id].parseAll(""" x <- func()""").right.value should be(
      NonEmptyList.of(Extract[Id, HNil]("x", FuncCall[Id, HNil]("func", Nil, HNil), HNil))
    )
    FuncOp.body[Id].parseAll(""" x <- func()
                               | Peer 3""".stripMargin).right.value should be(
      NonEmptyList.of(
        Extract[Id, HNil]("x", FuncCall[Id, HNil]("func", Nil, HNil), HNil),
        AbilityId[Id, HNil]("Peer", Literal[Id]("3", BasicType.number), HNil)
      )
    )
    FuncOp.body[Id].parseAll(""" x <- func()
                               | on x:
                               |   Peer 3""".stripMargin).right.value should be(
      NonEmptyList.of(
        Extract[Id, HNil]("x", FuncCall[Id, HNil]("func", Nil, HNil), HNil),
        On[Id, HNil](
          VarLambda[Id]("x"),
          NonEmptyList.of(AbilityId[Id, HNil]("Peer", Literal[Id]("3", BasicType.number), HNil)),
          HNil
        )
      )
    )
    FuncOp.body[Id].parseAll(""" on x:
                               |   Peer 3""".stripMargin).right.value should be(
      NonEmptyList.of(
        On[Id, HNil](
          VarLambda[Id]("x"),
          NonEmptyList.of(AbilityId[Id, HNil]("Peer", Literal[Id]("3", BasicType.number), HNil)),
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
   */

  /*

  On(VarLambda(peer,Some(id)),NonEmptyList(Extract(x,AbilityFuncCall(Ab,FuncCall(func,List()))), AbilityId(Peer,Literal("some id" call(true),List(BasicType(string)))))) was not equal to
  On(VarLambda(peer,Some(id)),NonEmptyList(Extract(x,AbilityFuncCall(Ab,FuncCall(func,List()))), AbilityId(Peer,Literal("some id",List(BasicType(string)))), FuncCall(call,List(Literal(true,List(BasicType(bool)))))))

   */
}
