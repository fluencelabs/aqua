package aqua.parser.lexer

import aqua.parser.lift.LiftParser.Implicits.idLiftParser
import aqua.types.ScalarType
import aqua.types.ScalarType.u32
import cats.Id
import cats.parse.Parser
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

class TypeTokenSpec extends AnyFlatSpec with Matchers with EitherValues {

  import aqua.AquaSpec._

  implicit def strToBt(st: ScalarType): BasicTypeToken[Id] = BasicTypeToken[Id](st)

  "Basic type" should "parse" in {
    BasicTypeToken.`basictypedef`.parseAll("u32").value.mapK(spanToId) should be(strToBt(u32))
    BasicTypeToken.`basictypedef`.parseAll("()").isLeft should be(true)
  }

  "Return type" should "parse" in {

    def typedef(str: String) =
      ArrowTypeToken.typeDef().parseAll(str).value.mapK(spanToId)

    def returndef(str: String) =
      ArrowTypeToken.returnDef().parseAll(str).value.map(_.mapK(spanToId))

    typedef("(A -> ())") should be(
      ArrowTypeToken[Id]((), List((None, NamedTypeToken[Id]("A"))), Nil)
    )
    typedef("(A -> B)") should be(
      ArrowTypeToken[Id]((), List((None, NamedTypeToken[Id]("A"))), List(NamedTypeToken[Id]("B")))
    )

    returndef("(A -> B), (C -> D)") should be(
      List(
        ArrowTypeToken[Id](
          (),
          (None, NamedTypeToken[Id]("A")) :: Nil,
          List(NamedTypeToken[Id]("B"))
        ),
        ArrowTypeToken[Id](
          (),
          (None, NamedTypeToken[Id]("C")) :: Nil,
          List(NamedTypeToken[Id]("D"))
        )
      )
    )

    returndef("A, (B, C -> D, E), F -> G, H") should be(
      List(
        NamedTypeToken[Id]("A"),
        ArrowTypeToken[Id](
          (),
          (None, NamedTypeToken[Id]("B")) :: (None, NamedTypeToken[Id]("C")) :: Nil,
          List(NamedTypeToken[Id]("D"), NamedTypeToken[Id]("E"))
        ),
        ArrowTypeToken[Id](
          (),
          (None, NamedTypeToken[Id]("F")) :: Nil,
          List(NamedTypeToken[Id]("G"), NamedTypeToken[Id]("H"))
        )
      )
    )
  }

  "Arrow type" should "parse" in {
    def arrowdef(str: String) =
      ArrowTypeToken.`arrowdef`(DataTypeToken.`datatypedef`).parseAll(str).value.mapK(spanToId)
    def arrowWithNames(str: String) = ArrowTypeToken
      .`arrowWithNames`(DataTypeToken.`datatypedef`)
      .parseAll(str)
      .value
      .mapK(spanToId)

    arrowdef("-> B") should be(
      ArrowTypeToken[Id]((), Nil, List(NamedTypeToken[Id]("B")))
    )
    arrowdef("A -> B") should be(
      ArrowTypeToken[Id](
        (),
        (None -> NamedTypeToken[Id]("A")) :: Nil,
        List(NamedTypeToken[Id]("B"))
      )
    )

    arrowdef("A -> B -> C") should be(
      ArrowTypeToken[Id](
        (),
        (None -> NamedTypeToken[Id]("A")) :: Nil,
        List(
          ArrowTypeToken[Id](
            (),
            (None -> NamedTypeToken[Id]("B")) :: Nil,
            List(NamedTypeToken[Id]("C"))
          )
        )
      )
    )

    arrowdef("A -> B, C -> D") should be(
      ArrowTypeToken[Id](
        (),
        (None -> NamedTypeToken[Id]("A")) :: Nil,
        List(
          ArrowTypeToken[Id](
            (),
            (None -> NamedTypeToken[Id]("B")) :: (None -> NamedTypeToken[Id]("C")) :: Nil,
            List(NamedTypeToken[Id]("D"))
          )
        )
      )
    )

    arrowdef("A -> (B -> F), (C -> D, E)") should be(
      ArrowTypeToken[Id](
        (),
        (None -> NamedTypeToken[Id]("A")) :: Nil,
        List(
          ArrowTypeToken[Id](
            (),
            (None -> NamedTypeToken[Id]("B")) :: Nil,
            NamedTypeToken[Id]("F") :: Nil
          ),
          ArrowTypeToken[Id](
            (),
            (None -> NamedTypeToken[Id]("C")) :: Nil,
            NamedTypeToken[Id]("D") :: NamedTypeToken[Id]("E") :: Nil
          )
        )
      )
    )

    arrowWithNames("(a: A) -> B") should be(
      ArrowTypeToken[Id](
        (),
        (Some(Name[Id]("a")) -> NamedTypeToken[Id]("A")) :: Nil,
        List(NamedTypeToken[Id]("B"))
      )
    )

    arrowWithNames("{SomeAb, SecondAb}(a: A) -> B") should be(
      ArrowTypeToken[Id](
        (),
        (Some(Name[Id]("SomeAb")) -> NamedTypeToken[Id]("SomeAb")) :: (Some(Name[Id](
          "SecondAb"
        )) -> NamedTypeToken[Id]("SecondAb")) :: (
          Some(Name[Id]("a")) -> NamedTypeToken[Id]("A")
        ) :: Nil,
        List(NamedTypeToken[Id]("B"))
      )
    )

    arrowdef("u32 -> Boo") should be(
      ArrowTypeToken[Id](
        (),
        (None -> strToBt(u32)) :: Nil,
        List(NamedTypeToken[Id]("Boo"))
      )
    )
    TypeToken.`typedef`.parseAll("u32 -> ()").value.mapK(spanToId) should be(
      ArrowTypeToken[Id]((), (None -> strToBt(u32)) :: Nil, Nil)
    )
    arrowdef("A, u32 -> B") should be(
      ArrowTypeToken[Id](
        (),
        (None -> NamedTypeToken[Id]("A")) :: (None -> strToBt(u32)) :: Nil,
        List(NamedTypeToken[Id]("B"))
      )
    )
    arrowdef("[]Absolutely, u32 -> B, C") should be(
      ArrowTypeToken[Id](
        (),
        (Option.empty[Name[Id]] -> ArrayTypeToken[Id]((), NamedTypeToken[Id]("Absolutely"))) ::
          (Option.empty[Name[Id]] -> strToBt(u32)) :: Nil,
        NamedTypeToken[Id]("B") ::
          NamedTypeToken[Id]("C") :: Nil
      )
    )

  }

  "Array type" should "parse" in {
    def typedef(str: String) = TypeToken.`typedef`.parseAll(str).value.mapK(spanToId)

    typedef("[]Something") should be(
      ArrayTypeToken[Id]((), NamedTypeToken[Id]("Something"))
    )
    typedef("[]u32") should be(
      ArrayTypeToken[Id]((), strToBt(u32))
    )
    typedef("[][]u32") should be(
      ArrayTypeToken[Id]((), ArrayTypeToken[Id]((), strToBt(u32)))
    )
  }

}
