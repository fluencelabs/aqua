package aqua.parse

import aqua.parse.lexer.{Literal, VarLambda}
import cats.data.NonEmptyList
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import aqua.parse.lift.LiftParser.Implicits.idLiftParser
import cats.Id

class FuncSpec extends AnyFlatSpec with Matchers with EitherValues {

  private val getTimeHead = FuncHead[Id](
    "getTime",
    Map("peer" -> CustomType("PeerId"), "ret" -> ArrowType(BasicType("i32") :: Nil, BasicType("()"))),
    Some(BasicType("string"))
  )

  "func header" should "parse" in {
    DefFunc.`funchead`.parseAll("func some()").right.value should be(FuncHead("some", Map.empty, None))
    DefFunc.`funchead`.parseAll("func some(peer: i32)").right.value should be(
      FuncHead("some", Map("peer" -> BasicType("i32")), None)
    )

    DefFunc.`funchead`.parseAll("func some(peer: PeerId)").right.value should be(
      FuncHead("some", Map("peer" -> CustomType("PeerId")), None)
    )
    DefFunc.`funchead`.parseAll("func some(peer: PeerId, other: i32)").right.value should be(
      FuncHead("some", Map("peer" -> CustomType("PeerId"), "other" -> BasicType("i32")), None)
    )
    DefFunc.`funchead`.parseAll("func some(peer: PeerId, other: i32 -> i32)").right.value should be(
      FuncHead(
        "some",
        Map("peer" -> CustomType("PeerId"), "other" -> ArrowType(BasicType("i32") :: Nil, BasicType("i32"))),
        None
      )
    )

    DefFunc.`funchead`.parseAll("func getTime(peer: PeerId, ret: i32 -> ()) -> string").right.value should be(
      getTimeHead
    )
  }

  "function" should "parse getTime as a whole" in {
    val func =
      """func getTime(peer: PeerId, ret: i32 -> ()) -> string:
        | on peer:
        |   Peer "peer"
        |   t <- Peer.timestamp()
        | ret(t)""".stripMargin

    DefFunc.`deffunc`.parseAll(func).right.value should be(
      DefFunc[Id](
        getTimeHead,
        NonEmptyList.of(
          On[Id](
            VarLambda("peer", None),
            NonEmptyList.of(
              AbilityId[Id]("Peer", Literal("\"peer\"", BasicType.string)),
              Extract[Id]("t", AbilityFuncCall[Id]("Peer", FuncCall[Id]("timestamp", Nil)))
            )
          ),
          FuncCall[Id]("ret", VarLambda("t", None) :: Nil)
        )
      )
    )
  }
}
