package aqua.parser

import aqua.parser.lexer.{Literal, VarLambda}
import cats.data.NonEmptyList
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import aqua.parser.lift.LiftParser.Implicits.idLiftParser
import cats.Id

class FuncOpSpec extends AnyFlatSpec with Matchers with EitherValues {

  "func calls" should "parse func()" in {
    FuncOp.`funcop`.parseAll("func()") should be(Right(FuncCall("func", Nil)))
    FuncOp.`funcop`.parseAll("func(arg)") should be(Right(FuncCall[Id]("func", VarLambda("arg", None) :: Nil)))
    FuncOp.`funcop`.parseAll("func(arg.doSomeThing)") should be(
      Right(FuncCall[Id]("func", VarLambda("arg", Some("doSomeThing")) :: Nil))
    )
    FuncOp.`funcop`.parseAll("func(arg.doSomeThing, arg2)") should be(
      Right(FuncCall[Id]("func", VarLambda("arg", Some("doSomeThing")) :: VarLambda("arg2", None) :: Nil))
    )
  }

  "ability calls" should "parse Ab.func()" in {
    FuncOp.`funcop`.parseAll("Ab.func()") should be(Right(AbilityFuncCall[Id]("Ab", FuncCall[Id]("func", Nil))))
    FuncOp.`funcop`.parseAll("Ab.func(arg)") should be(
      Right(AbilityFuncCall[Id]("Ab", FuncCall[Id]("func", VarLambda("arg", None) :: Nil)))
    )
    FuncOp.`funcop`.parseAll("Ab.func(arg.doSomeThing)") should be(
      Right(AbilityFuncCall[Id]("Ab", FuncCall[Id]("func", VarLambda("arg", Some("doSomeThing")) :: Nil)))
    )
    FuncOp.`funcop`.parseAll("Ab.func(arg.doSomeThing, arg2)") should be(
      Right(
        AbilityFuncCall[Id](
          "Ab",
          FuncCall[Id]("func", VarLambda("arg", Some("doSomeThing")) :: VarLambda("arg2", None) :: Nil)
        )
      )
    )
  }

  "extracting" should "parse x <- func()" in {
    FuncOp.`funcop`.parseAll("someThing <- func()") should be(
      Right(Extract[Id]("someThing", FuncCall[Id]("func", Nil)))
    )
  }

  "extracting" should "parse x <- Ab.func()" in {
    val fCall = AbilityFuncCall[Id]("Ab", FuncCall[Id]("func", Nil))
    FuncOp.`funcop`.parseAll("x <- Ab.func()") should be(Right(Extract[Id]("x", fCall)))
  }

  // TODO test with literals
  "ability resolve" should "parse id getter" in {
    FuncOp.`funcop`.parseAll("Ab x") should be(Right(AbilityId[Id]("Ab", VarLambda("x", None))))
    FuncOp.`funcop`.parseAll("Ab x.id") should be(Right(AbilityId[Id]("Ab", VarLambda("x", Some("id")))))
  }

  "on" should "parse startOn" in {
    FuncOp.startOn.parseAll("on peer: \n") should be(Right(VarLambda("peer", None)))
    FuncOp.startOn.parseAll("on peer.id:\n") should be(Right(VarLambda("peer", Some("id"))))
  }

  "on" should "parse on x: y" in {
    val fCall = AbilityFuncCall[Id]("Ab", FuncCall[Id]("func", Nil))
    val extr = Extract[Id]("x", fCall)
    val resl = AbilityId[Id]("Peer", Literal("\"some id\"", BasicType.string))
    val call = FuncCall[Id]("call", Literal("true", BasicType.bool) :: Nil)

    val script = """on peer.id:
                   | x <- Ab.func()
                   | Peer "some id"
                   | call(true)""".stripMargin

    FuncOp.`funcop`.parseAll(script).right.value should be(
      On[Id](VarLambda("peer", Some("id")), NonEmptyList.of(extr, resl, call))
    )
  }

  "par" should "parse" in {
    FuncOp.`funcop`.parseAll("par func()") should be(Right(Par[Id](FuncCall[Id]("func", Nil))))

    val script = """par on peer.id:
                   |
                   | x <- Ab.func()
                   | call(smth)""".stripMargin

    FuncOp.`funcop`.parseAll(script) should be('right)
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
