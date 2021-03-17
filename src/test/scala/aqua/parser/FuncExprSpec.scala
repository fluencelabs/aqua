package aqua.parser

import aqua.ast.Indent
import aqua.ast.algebra.types.LiteralType
import aqua.ast.expr.{AbilityIdExpr, CoalgebraExpr, FuncExpr}
import aqua.parser.lexer.{Ability, IntoField, Literal, Name, VarLambda}
import cats.data.NonEmptyList
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import aqua.parser.lift.LiftParser.Implicits.idLiftParser
import cats.Id
import shapeless.HNil

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

  "func calls" should "parse func()" in {
    parseExpr("func()") should be(CoalgebraExpr[Id](None, None, toName("func"), List()))
    parseExpr("Ab.func(arg)") should be(
      CoalgebraExpr[Id](None, Some(toAb("Ab")), Name[Id]("func"), List(VarLambda[Id](toName("arg"))))
    )

    parseExpr("func(arg.doSomething)") should be(
      CoalgebraExpr[Id](None, None, Name[Id]("func"), List(toVar("arg", List("doSomething"))))
    )

    parseExpr("func(arg.doSomething.and.doSomethingElse)") should be(
      CoalgebraExpr[Id](None, None, Name[Id]("func"),
        List(toVar("arg", List("doSomething", "and", "doSomethingElse")))
      )
    )

    parseExpr("Ab.func(arg.doSomething.and.doSomethingElse, arg2.someFunc)") should be(
      CoalgebraExpr[Id](None, Some(toAb("Ab")), Name[Id]("func"),
        List(toVar("arg", List("doSomething", "and", "doSomethingElse")),
          toVar("arg2", List("someFunc"))
        )
      )
    )

    parseExpr("x <- func(arg.doSomething)") should be(
      CoalgebraExpr[Id](Some(toName("x")), None, Name[Id]("func"),
        List(
          toVar("arg", List("doSomething"))
        )
      )
    )
  }

  "abilities" should "be parsed" in {
    // TODO test with literals
    parseAbId("Ab a") should be(
      AbilityIdExpr[Id](toAb("Ab"), toVar("a", List()),
      )
    )

    parseAbId("Ab a.id") should be(
      AbilityIdExpr[Id](toAb("Ab"), toVar("a", List("id")),
      )
    )
  }

    /*
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
    }*/

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
