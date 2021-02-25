package aqua.parse

import cats.data.NonEmptyList
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FuncOpSpec extends AnyFlatSpec with Matchers with EitherValues{
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

Ability "const"
Ability service.id
Ability:
  // we're on a local node
  call smth
  res.id

func {Need, x}(y: i32, z: {x}i64 -> Y, p: -> {x}) -> {x}Bool:

Ability keyword, parser
Literal, true/false
AbilityResolve: literal, lens, or arrow that resolves to a string id

try:
 ...
catch( errn)?:
 ...


if a == != b:

else:

   */

  "func calls" should "parse func()" in {
    FuncOp.`funcop`.parseAll("func()") should be(Right(FuncCall("func", Nil)))
    FuncOp.`funcop`.parseAll("func(arg)") should be(Right(FuncCall("func", VarLambda("arg", None) :: Nil)))
    FuncOp.`funcop`.parseAll("func(arg.doSomeThing)") should be(Right(FuncCall("func", VarLambda("arg", Some("doSomeThing")) :: Nil)))
    FuncOp.`funcop`.parseAll("func(arg.doSomeThing, arg2)") should be(Right(FuncCall("func", VarLambda("arg", Some("doSomeThing")) :: VarLambda("arg2", None) :: Nil)))
  }

  "ability calls" should "parse Ab.func()" in {
    FuncOp.`funcop`.parseAll("Ab.func()") should be(Right(AbilityFuncCall("Ab", FuncCall("func", Nil))))
    FuncOp.`funcop`.parseAll("Ab.func(arg)") should be(Right(AbilityFuncCall("Ab", FuncCall("func", VarLambda("arg", None) :: Nil))))
    FuncOp.`funcop`.parseAll("Ab.func(arg.doSomeThing)") should be(Right(AbilityFuncCall("Ab", FuncCall("func", VarLambda("arg", Some("doSomeThing")) :: Nil))))
    FuncOp.`funcop`.parseAll("Ab.func(arg.doSomeThing, arg2)") should be(Right(AbilityFuncCall("Ab", FuncCall("func", VarLambda("arg", Some("doSomeThing")) :: VarLambda("arg2", None) :: Nil))))
  }

  "extracting" should "parse x <- func()" in {
    FuncOp.`funcop`.parseAll("someThing <- func()") should be(Right(Extract("someThing", FuncCall("func", Nil))))
  }

  "extracting" should "parse x <- Ab.func()" in {
    val fCall = AbilityFuncCall("Ab", FuncCall("func", Nil))
    FuncOp.`funcop`.parseAll("x <- Ab.func()") should be(Right(Extract("x", fCall)))
  }

  // TODO test with literals
  "ability resolve" should "parse id getter" in {
    FuncOp.`funcop`.parseAll("Ab x") should be(Right(AbilityId("Ab", VarLambda("x", None))))
    FuncOp.`funcop`.parseAll("Ab x.id") should be(Right(AbilityId("Ab", VarLambda("x", Some("id")))))
  }

  "on" should "parse startOn" in {
    FuncOp.startOn.parseAll("on peer: \n") should be(Right(VarLambda("peer", None)))
    FuncOp.startOn.parseAll("on peer.id:\n") should be(Right(VarLambda("peer", Some("id"))))
  }

  "on" should "parse on x: y" in {
    val fCall = AbilityFuncCall("Ab", FuncCall("func", Nil))
    val extr = Extract("x", fCall)
    val call = FuncCall("call", VarLambda("smth", None) :: Nil)

    val script = """on peer.id:
                   | x <- Ab.func()
                   | call(smth)""".stripMargin

    FuncOp.`funcop`.parseAll(script).right.value should be(On(VarLambda("peer", Some("id")), NonEmptyList.of(extr, call)))
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
