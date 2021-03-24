package aqua.parser

import aqua.Utils
import aqua.semantics.ScalarType.{bool, u64}
import aqua.parser.expr.FuncExpr
import aqua.parser.lexer.{
  Ability,
  Arg,
  ArrowTypeToken,
  BasicTypeToken,
  CustomTypeToken,
  Literal,
  Name,
  TypeToken,
  VarLambda
}
import cats.data.NonEmptyList
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import aqua.parser.lift.LiftParser.Implicits.idLiftParser
import aqua.semantics.ScalarType
import cats.Id

import scala.language.implicitConversions

class FuncSpec extends AnyFlatSpec with Matchers with Utils {
  import Utils._
  import aqua.semantics.ScalarType.{string, u32}

  "func header" should "parse" in {
    funcExpr("func some() -> bool") should be(FuncExpr("some", List(), Some(bool: BasicTypeToken[Id]), None))
    funcExpr("func some()") should be(FuncExpr("some", List(), None, None))

    val arrowToken = ArrowTypeToken[Id]((), List(BasicTypeToken[Id](u32)), Some(BasicTypeToken[Id](bool)))
    funcExpr("func some(peer: PeerId, other: u32 -> bool)") should be(
      FuncExpr(toName("some"), List(toCustomArg("peer", "PeerId"), toArg("other", arrowToken)), None, None)
    )

    val arrowToken2 =
      ArrowTypeToken[Id]((), List(BasicTypeToken[Id](u32), BasicTypeToken[Id](u64)), Some(BasicTypeToken[Id](bool)))
    funcExpr("func some(peer: PeerId, other: u32, u64 -> bool)") should be(
      FuncExpr(toName("some"), List(toCustomArg("peer", "PeerId"), toArg("other", arrowToken2)), None, None)
    )

    val arrowToken3 = ArrowTypeToken[Id]((), List(BasicTypeToken[Id](u32)), None)
    funcExpr("func getTime(peer: PeerId, ret: u32 -> ()) -> string") should be(
      FuncExpr(
        toName("getTime"),
        List(toCustomArg("peer", "PeerId"), toArg("ret", arrowToken3)),
        Some(BasicTypeToken[Id](string)),
        None
      )
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
