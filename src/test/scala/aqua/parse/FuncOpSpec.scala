package aqua.parse

import cats.data.NonEmptyList
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FuncOpSpec extends AnyFlatSpec with Matchers{
  /**
   * line variants:
func(...)
x <- func(...)
Ability.func(...)
x <- Ability.func(...)

par line
xor line

on peer:
  indented lines*
   */

  "func calls" should "parse func()" in {
    FuncOp.`funcop`.parseAll("func()") should be(Right(FuncCall("func", Nil)))
    FuncOp.`funcop`.parseAll("func(arg)") should be(Right(FuncCall("func", VarLambda("arg", None) :: Nil)))
    FuncOp.`funcop`.parseAll("func(arg.doSomeThing)") should be(Right(FuncCall("func", VarLambda("arg", Some("doSomeThing")) :: Nil)))
    FuncOp.`funcop`.parseAll("func(arg.doSomeThing, arg2)") should be(Right(FuncCall("func", VarLambda("arg", Some("doSomeThing")) :: VarLambda("arg2", None) :: Nil)))
  }

  "ability calls" should "parse Ab.func()" in {
    FuncOp.`funcop`.parseAll("Ab.func()") should be(Right(AbilityFuncCall(CustomType("Ab"), FuncCall("func", Nil))))
    FuncOp.`funcop`.parseAll("Ab.func(arg)") should be(Right(AbilityFuncCall(CustomType("Ab"), FuncCall("func", VarLambda("arg", None) :: Nil))))
    FuncOp.`funcop`.parseAll("Ab.func(arg.doSomeThing)") should be(Right(AbilityFuncCall(CustomType("Ab"), FuncCall("func", VarLambda("arg", Some("doSomeThing")) :: Nil))))
    FuncOp.`funcop`.parseAll("Ab.func(arg.doSomeThing, arg2)") should be(Right(AbilityFuncCall(CustomType("Ab"), FuncCall("func", VarLambda("arg", Some("doSomeThing")) :: VarLambda("arg2", None) :: Nil))))
  }

  "extracting" should "parse x <- func()" in {
    FuncOp.`funcop`.parseAll("someThing <- func()") should be(Right(Extract("someThing", FuncCall("func", Nil))))
  }

  "extracting" should "parse x <- Ab.func()" in {
    val fCall = AbilityFuncCall(CustomType("Ab"), FuncCall("func", Nil))
    FuncOp.`funcop`.parseAll("x <- Ab.func()") should be(Right(Extract("x", fCall)))
  }

  "on" should "parse startOn" in {
    FuncOp.startOn.parseAll("on peer: \n") should be(Right(VarLambda("peer", None)))
    FuncOp.startOn.parseAll("on peer.id: \n") should be(Right(VarLambda("peer", Some("id"))))
  }

  "on" should "parse on x: y" in {
    val fCall = AbilityFuncCall(CustomType("Ab"), FuncCall("func", Nil))
    val extr = Extract("x", fCall)
    val call = FuncCall("call", VarLambda("smth", None) :: Nil)

    val script = """on peer.id:
                   | x <- Ab.func()
                   | call(smth)""".stripMargin

    FuncOp.`funcop`.parseAll(script) should be(Right(On(VarLambda("peer", Some("id")), NonEmptyList.of(extr, call))))
  }

  "par" should "parse" in {
    FuncOp.`funcop`.parseAll("par func()") should be(Right(Par(FuncCall("func", Nil))))

    val script = """par on peer.id:
                   |
                   | x <- Ab.func()
                   | call(smth)""".stripMargin

    FuncOp.`funcop`.parseAll(script) should be('right)
  }
}
