package aqua.parser

import aqua.parser.lexer.{Ability, ArrowName, ArrowType, BasicType, CustomType, Literal, Var, VarLambda}
import cats.data.NonEmptyList
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import aqua.parser.lift.LiftParser.Implicits.idLiftParser
import cats.Id
import shapeless.HNil

import scala.language.implicitConversions

class FuncSpec extends AnyFlatSpec with Matchers with EitherValues {

  implicit def strToBt(str: String): BasicType[Id] = BasicType[Id](BasicType.Value(str))
  implicit def strToArrow(str: String): ArrowName[Id] = ArrowName[Id](str)
  implicit def strToAb(str: String): Ability[Id] = Ability[Id](str)
  implicit def strToVar(str: String): Var[Id] = Var[Id](str)

  private val getTimeHead = FuncHead[Id](
    "getTime",
    List(
      ("peer", "peer", CustomType[Id]("PeerId")),
      ("ret", "ret", ArrowType[Id](("i32": BasicType[Id]) :: Nil, "()": BasicType[Id]))
    ),
    Some("string": BasicType[Id])
  )

  "func header" should "parse" in {
    DefFunc.`funchead`.parseAll("func some()").right.value should be(FuncHead("some", Nil, None))
    DefFunc.`funchead`.parseAll("func some(peer: i32)").right.value should be(
      FuncHead[Id]("some", List(("peer", "peer", ("i32": BasicType[Id]))), None)
    )

    DefFunc.`funchead`.parseAll("func some(peer: PeerId)").right.value should be(
      FuncHead[Id]("some", List(("peer", "peer", CustomType[Id]("PeerId"))), None)
    )
    DefFunc.`funchead`.parseAll("func some(peer: PeerId, other: i32)").right.value should be(
      FuncHead[Id](
        "some",
        List(("peer", "peer", CustomType[Id]("PeerId")), ("other", "other", ("i32": BasicType[Id]))),
        None
      )
    )
    DefFunc.`funchead`.parseAll("func some(peer: PeerId, other: i32 -> i32)").right.value should be(
      FuncHead[Id](
        "some",
        List(
          ("peer", "peer", CustomType[Id]("PeerId")),
          ("other", "other", ArrowType[Id](("i32": BasicType[Id]) :: Nil, ("i32": BasicType[Id])))
        ),
        None
      )
    )

    DefFunc.`funchead`.parseAll("func getTime(peer: PeerId, ret: i32 -> ()) -> string").right.value should be(
      getTimeHead
    )
  }

  "function" should "parse single line fn" in {
    val func =
      """func getTime(peer: PeerId, ret: i32 -> ()) -> string:
        | ret(43)""".stripMargin

    DefFunc.`deffunc`.parseAll(func).right.value should be(
      DefFunc[Id, HNil](
        getTimeHead,
        NonEmptyList.of(
          FuncCall[Id, HNil]("ret", Literal[Id]("43", BasicType.number) :: Nil, HNil)
        ),
        HNil
      )
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
      DefFunc[Id, HNil](
        getTimeHead,
        NonEmptyList.of(
          On[Id, HNil](
            VarLambda[Id]("peer", Nil),
            NonEmptyList.of(
              AbilityId[Id, HNil]("Peer", Literal[Id]("\"peer\"", BasicType.string), HNil),
              Extract[Id, HNil]("t", AbilityFuncCall[Id, HNil]("Peer", "timestamp", Nil, HNil), HNil)
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
      """func getTime(peer: PeerId, ret: i32 -> ()) -> string:
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
              AbilityId[Id, HNil]("Peer", Literal[Id]("\"peer\"", BasicType.string), HNil),
              Extract[Id, HNil]("t", AbilityFuncCall[Id, HNil]("Peer", "timestamp", Nil, HNil), HNil)
            ),
            HNil
          )
        ),
        HNil
      )
    )
  }
}
