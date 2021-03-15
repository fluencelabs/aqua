package aqua.parser

import aqua.ast.algebra.types.{LiteralType, ScalarType}
import aqua.parser.lexer.{Ability, ArrowTypeToken, BasicTypeToken, CustomTypeToken, Literal, Name, VarLambda}
import cats.data.NonEmptyList
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import aqua.parser.lift.LiftParser.Implicits.idLiftParser
import cats.Id
import shapeless.HNil

import scala.language.implicitConversions

class FuncSpec extends AnyFlatSpec with Matchers with EitherValues {

  import aqua.ast.algebra.types.ScalarType.{string, u32}

  implicit def scToBt(sc: ScalarType): BasicTypeToken[Id] = BasicTypeToken[Id](sc)
  implicit def strToAb(str: String): Ability[Id] = Ability[Id](str)
  implicit def strToVar(str: String): Name[Id] = Name[Id](str)
  /*
  private val getTimeHead = FuncHead[Id](
    "getTime",
    List(
      ("peer", "peer", CustomTypeToken[Id]("PeerId")),
      ("ret", "ret", ArrowTypeToken[Id]((), (u32: BasicTypeToken[Id]) :: Nil, None))
    ),
    Some(string: BasicTypeToken[Id])
  )

  "func header" should "parse" in {
    DefFunc.`funchead`.parseAll("func some()").right.value should be(FuncHead("some", Nil, None))
    DefFunc.`funchead`.parseAll("func some(peer: u32)").right.value should be(
      FuncHead[Id]("some", List(("peer", "peer", (u32: BasicTypeToken[Id]))), None)
    )

    DefFunc.`funchead`.parseAll("func some(peer: PeerId)").right.value should be(
      FuncHead[Id]("some", List(("peer", "peer", CustomTypeToken[Id]("PeerId"))), None)
    )
    DefFunc.`funchead`.parseAll("func some(peer: PeerId, other: u32)").right.value should be(
      FuncHead[Id](
        "some",
        List(("peer", "peer", CustomTypeToken[Id]("PeerId")), ("other", "other", (u32: BasicTypeToken[Id]))),
        None
      )
    )
    DefFunc.`funchead`.parseAll("func some(peer: PeerId, other: u32 -> u32)").right.value should be(
      FuncHead[Id](
        "some",
        List(
          ("peer", "peer", CustomTypeToken[Id]("PeerId")),
          ("other", "other", ArrowTypeToken[Id]((), (u32: BasicTypeToken[Id]) :: Nil, Some(u32: BasicTypeToken[Id])))
        ),
        None
      )
    )

    DefFunc.`funchead`.parseAll("func getTime(peer: PeerId, ret: u32 -> ()) -> string").right.value should be(
      getTimeHead
    )
  }

  "function" should "parse single line fn" in {
    val func =
      """func getTime(peer: PeerId, ret: u32 -> ()) -> string:
        | ret(43)""".stripMargin

    DefFunc.`deffunc`.parseAll(func).right.value should be(
      DefFunc[Id, HNil](
        getTimeHead,
        NonEmptyList.of(
          FuncCall[Id, HNil]("ret", Literal[Id]("43", LiteralType.number) :: Nil, HNil)
        ),
        HNil
      )
    )
  }

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
