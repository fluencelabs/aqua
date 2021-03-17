package aqua.parser

import aqua.ast.algebra.types.ScalarType.{bool, u64}
import aqua.ast.algebra.types.{LiteralType, ScalarType}
import aqua.ast.expr.FuncExpr
import aqua.parser.lexer.{Ability, Arg, ArrowTypeToken, BasicTypeToken, CustomTypeToken, Literal, Name, TypeToken, VarLambda}
import cats.data.NonEmptyList
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import aqua.parser.lift.LiftParser.Implicits.idLiftParser
import cats.Id

import scala.language.implicitConversions

class FuncSpec extends AnyFlatSpec with Matchers with EitherValues {

  import aqua.ast.algebra.types.ScalarType.{string, u32}

  implicit def scToBt(sc: ScalarType): BasicTypeToken[Id] = BasicTypeToken[Id](sc)

  implicit def strToAb(str: String): Ability[Id] = Ability[Id](str)

  implicit def toName(str: String): Name[Id] = Name[Id](str)

  implicit def toCustomType(str: String): CustomTypeToken[Id] = CustomTypeToken[Id](str)

  implicit def toCustomArg(str: String, customType: String): Arg[Id] = Arg[Id](toName(str), toCustomType(customType))

  implicit def toArg(str: String, typeToken: TypeToken[Id]): Arg[Id] = Arg[Id](toName(str), typeToken)

  def funcExpr(str: String): FuncExpr[Id] = FuncExpr.p[Id].parseAll(str).value

  "func header" should "parse" in {
    funcExpr("func some() -> bool:\n") should be(FuncExpr(toName("some"), List(), Some(bool: BasicTypeToken[Id])))
    funcExpr("func some():\n") should be(FuncExpr(toName("some"), List(), None))

    val arrowToken = ArrowTypeToken[Id]((), List(BasicTypeToken[Id](u32)), Some(BasicTypeToken[Id](bool)))
    funcExpr("func some(peer: PeerId, other: u32 -> bool):\n") should be(
      FuncExpr(toName("some"), List(toCustomArg("peer", "PeerId"), toArg("other", arrowToken)), None)
    )

    val arrowToken2 = ArrowTypeToken[Id]((), List(BasicTypeToken[Id](u32), BasicTypeToken[Id](u64)), Some(BasicTypeToken[Id](bool)))
    funcExpr("func some(peer: PeerId, other: u32, u64 -> bool):\n") should be(
      FuncExpr(toName("some"), List(toCustomArg("peer", "PeerId"), toArg("other", arrowToken2)), None)
    )

    val arrowToken3 = ArrowTypeToken[Id]((), List(BasicTypeToken[Id](u32)), None)
    funcExpr("func getTime(peer: PeerId, ret: u32 -> ()) -> string:\n") should be(
      FuncExpr(toName("getTime"), List(toCustomArg("peer", "PeerId"), toArg("ret", arrowToken3)), Some(BasicTypeToken[Id](string)))
    )

  }
  /*

    "function" should "parse getTime as a whole" in {
      val func =
        """func getTime(peer: PeerId, ret: u32 -> ()) -> string:
          | on peer:
          |   Peer "peer"
          |   t <- Peer.timestamp()
          | ret(t)""".stripMargin

      DefFunc.`deffunc`.parseAll(func).right.value should be(
        DefFunc[Id, HNil](
          getTimeHead,
          NonEmptyList.of(
            On[Id, HNil](
              VarLambda[Id]("peer", Nil),
              NonEmptyList.of(
                AbilityId[Id, HNil]("Peer", Literal[Id]("\"peer\"", LiteralType.string), HNil),
                Extract[Id, HNil]("t", AbilityFuncCall[Id, HNil]("Peer", "timestamp", "Peer.timestamp", Nil, HNil), HNil)
              ),
              HNil
            ),
            FuncCall[Id, HNil]("ret", VarLambda[Id]("t", Nil) :: Nil, HNil)
          ),
          HNil
        )
      )
    }

    "function" should "parse getTime with no return" in {
      val func =
        """func getTime(peer: PeerId, ret: u32 -> ()) -> string:
          | on peer:
          |   Peer "peer"
          |   t <- Peer.timestamp()""".stripMargin

      DefFunc.`deffunc`.parseAll(func).right.value should be(
        DefFunc[Id, HNil](
          getTimeHead,
          NonEmptyList.of(
            On[Id, HNil](
              VarLambda[Id]("peer", Nil),
              NonEmptyList.of(
                AbilityId[Id, HNil]("Peer", Literal[Id]("\"peer\"", LiteralType.string), HNil),
                Extract[Id, HNil]("t", AbilityFuncCall[Id, HNil]("Peer", "timestamp", "Peer.timestamp", Nil, HNil), HNil)
              ),
              HNil
            )
          ),
          HNil
        )
      )
    }*/
}
