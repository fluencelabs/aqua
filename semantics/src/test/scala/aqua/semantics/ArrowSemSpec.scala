package aqua.semantics

import aqua.parser.expr.func.ArrowExpr
import aqua.parser.lexer.{BasicTypeToken, Name}
import aqua.raw.Raw
import aqua.raw.arrow.ArrowRaw
import aqua.raw.ops.*
import aqua.raw.value.LiteralRaw
import aqua.semantics.expr.func.ArrowSem
import aqua.semantics.rules.types.TypesState
import aqua.types.*
import aqua.types.ScalarType.*

import cats.Id
import cats.data.{NonEmptyList, NonEmptyMap, State}
import cats.syntax.applicative.*
import org.scalatest.EitherValues
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ArrowSemSpec extends AnyFlatSpec with Matchers with EitherValues with Inside {

  import Utils.{given, *}

  def program(arrowStr: String): Prog[State[CompilerState[cats.Id], *], Raw] = {
    import CompilerState.*

    val expr = ArrowExpr.p.parseAll(arrowStr).value.mapK(spanToId)
    val sem = new ArrowSem[Id](expr)

    sem.program[State[CompilerState[Id], *]]
  }

  "ArrowSem" should "create empty model" in {
    val model = getModel(program("(a: string, b: u32) -> u8"))
    model shouldBe (Raw.Empty("Invalid arrow body"))
  }

  it should "create error model" ignore {
    val model = getModel(RawTag.empty.toFuncOp)(program("(a: string, b: u32) -> u8"))
    model shouldBe Raw.Empty(
      "Return type is defined for the arrow, but nothing returned. Use `<- value, ...` as the last expression inside function body."
    )
  }

  it should "create right model for arrow without return type" ignore {
    val model = getModel(RawTag.empty.toFuncOp)(program("(a: string, b: u32)"))
    model shouldBe ArrowRaw(
      ArrowType(labelled("a", string, labelled("b", u32)), NilType),
      Nil,
      RawTag.empty
    )
  }

  it should "create correct model for arrow with return type and correct state" ignore {
    val returnValue = LiteralRaw("123", string)
    val returnTag = FuncOp(ReturnTag(NonEmptyList.one(returnValue)).wrap(RawTag.empty))
    val model = getModel(returnTag)(program("(a: string, b: u32) -> string"))

    val arrowType = ArrowType(labelled("a", string, labelled("b", u32)), productType(string))
    val resultModel = ArrowRaw(arrowType, returnValue :: Nil, returnTag.tree)
    model shouldBe resultModel
  }

  it should "create correct model for arrow with return type and seq inside" ignore {
    val returnValue = LiteralRaw("123", string)
    val seq = FuncOp(SeqTag.wrap(RawTag.empty, ReturnTag(NonEmptyList.one(returnValue)).leaf))
    val model = getModel(seq)(program("(a: string, b: u32) -> string"))

    val arrowType = ArrowType(labelled("a", string, labelled("b", u32)), productType(string))
    val resultModel = ArrowRaw(arrowType, returnValue :: Nil, seq.tree)
    model shouldBe resultModel
  }

  it should "create error model for different types in return type and return value" ignore {
    val returnValue = LiteralRaw("123", string)
    val seq = FuncOp(
      SeqTag.wrap(
        RawTag.empty,
        ReturnTag(NonEmptyList.one(returnValue)).leaf
      )
    )
    val state = getState(seq)(program("(a: string, b: u32) -> u32"))

    state.errors.headOption.get shouldBe RulesViolated[Id](
      BasicTypeToken[Id](u32),
      "Types mismatch, expected: u32, given: string" :: Nil
    )

  }

  it should "not restrict stream arguments" in {
    val body =
      PushToStreamTag(
        LiteralRaw.quote("a"),
        Call.Export("s", StreamType(ScalarType.string))
      ).leaf

    def test(
      abilityArgs: List[String],
      args: List[String],
      initState: CompilerState[Id]
    ) = {
      val abilityPart =
        if (abilityArgs.isEmpty) ""
        else abilityArgs.mkString("{", ",", "}")
      val argsPart = args.appended("s: *string").mkString("(", ",", ")")
      val expr = abilityPart + argsPart

      val (state, raw) = program(expr).apply(body.toFuncOp.pure).run(initState).value

      state.errors shouldBe empty
      inside(raw) { case ArrowRaw(_, Nil, bodyRes) =>
        bodyRes.equalsOrShowDiff(body) shouldBe true
      }
    }

    val structType = StructType(
      "TestStruct",
      NonEmptyMap.one(
        "field",
        ScalarType.string
      )
    )

    val abilityType = AbilityType(
      "TestAbility",
      NonEmptyMap.one(
        "field",
        ScalarType.string
      )
    )

    val state: CompilerState[Id] = CompilerState(
      types = TypesState(
        strict = Map(
          "FirstAbility" -> abilityType,
          "SecondAbility" -> abilityType,
          "DataStruct" -> structType
        )
      )
    )

    val abilityArgs = List(
      "FirstAbility",
      "SecondAbility"
    )

    val args = List(
      "a: string",
      "callback: u32 -> string",
      "data: DataStruct"
    )

    for {
      abilityArgs <- abilityArgs.toSet.subsets()
      args <- args.toSet.subsets()
    } test(abilityArgs.toList, args.toList, state)
  }

}
