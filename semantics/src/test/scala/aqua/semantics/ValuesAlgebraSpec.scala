package aqua.semantics

import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.names.NamesState
import aqua.semantics.rules.abilities.AbilitiesState
import aqua.semantics.rules.types.TypesState
import aqua.semantics.rules.types.TypesAlgebra
import aqua.semantics.rules.abilities.AbilitiesInterpreter
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.definitions.DefinitionsAlgebra
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.names.NamesInterpreter
import aqua.semantics.rules.definitions.DefinitionsInterpreter
import aqua.semantics.rules.types.TypesInterpreter
import aqua.semantics.rules.locations.LocationsAlgebra
import aqua.semantics.rules.locations.DummyLocationsInterpreter
import aqua.raw.value.{ApplyBinaryOpRaw, LiteralRaw}
import aqua.raw.RawContext
import aqua.types.*
import aqua.parser.lexer.{InfixToken, LiteralToken, Name, PrefixToken, ValueToken, VarToken}
import aqua.raw.value.ApplyUnaryOpRaw
import aqua.parser.lexer.ValueToken.string

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.Inside
import cats.Id
import cats.data.State
import cats.syntax.functor.*
import cats.syntax.comonad.*
import cats.data.NonEmptyMap
import monocle.syntax.all.*
import scala.collection.immutable.SortedMap

class ValuesAlgebraSpec extends AnyFlatSpec with Matchers with Inside {

  type TestState = CompilerState[Id]

  def algebra() = {
    type Interpreter[A] = State[TestState, A]

    given LocationsAlgebra[Id, Interpreter] =
      new DummyLocationsInterpreter[Id, CompilerState[Id]]

    given TypesAlgebra[Id, Interpreter] =
      new TypesInterpreter[Id, CompilerState[Id]]
    given AbilitiesAlgebra[Id, Interpreter] =
      new AbilitiesInterpreter[Id, CompilerState[Id]]
    given NamesAlgebra[Id, Interpreter] =
      new NamesInterpreter[Id, CompilerState[Id]]
    given DefinitionsAlgebra[Id, Interpreter] =
      new DefinitionsInterpreter[Id, CompilerState[Id]]

    new ValuesAlgebra[Id, Interpreter]
  }

  def literal(value: String, `type`: LiteralType) =
    LiteralToken(Id(value), `type`)

  def variable(name: String): VarToken[Id] =
    VarToken[Id](Name[Id](name))

  def allPairs[A](list: List[A]): List[(A, A)] = for {
    a <- list
    b <- list
  } yield (a, b)

  def genState(vars: Map[String, Type] = Map.empty) =
    CompilerState
      .init[Id](RawContext.blank)
      .focus(_.names)
      .modify(
        _.focus(_.stack).modify(
          NamesState.Frame(
            token = Name(Id("test")), // Token just for test
            names = vars
          ) :: _
        )
      )

  def valueOfType(t: Type)(
    varName: String,
    bool: String = "true",
    unsigned: String = "42",
    signed: String = "-42",
    string: String = "string"
  ): ValueToken[Id] = t match {
    case t: LiteralType if t == LiteralType.bool =>
      literal(bool, t)
    case t: LiteralType if t == LiteralType.unsigned =>
      literal(unsigned, t)
    case t: LiteralType if t == LiteralType.signed =>
      literal(signed, t)
    case t: LiteralType if t == LiteralType.string =>
      literal(f"\"$string\"", t)
    case _ =>
      variable(varName)
  }

  "valueToRaw" should "handle +, -, /, *, % on number literals" in {
    val types = List(
      LiteralType.signed,
      LiteralType.unsigned
    )

    allPairs(types).foreach { case (lt, rt) =>
      val llit = literal("42", lt)
      val rlit = literal("37", rt)

      val alg = algebra()

      InfixToken.Op.math
        .filterNot(
          // Can not use negative numbers with pow
          _ == InfixToken.Op.Pow && rt != LiteralType.unsigned
        )
        .foreach { op =>
          val token = InfixToken[Id](llit, rlit, op)

          val (st, res) = alg
            .valueToRaw(token)
            .run(genState())
            .value

          val t = if (lt == rt) lt else LiteralType.signed

          inside(res) { case Some(value) =>
            value.`type` shouldBe t
          }
        }
    }
  }

  it should "handle +, -, /, *, % on number vars" in {
    allPairs(ScalarType.integer.toList).foreach { case (lt, rt) =>
      val vl = variable("left")
      val vr = variable("right")

      val ut = lt.uniteTop(rt)

      val state = genState(
        vars = Map(
          "left" -> lt,
          "right" -> rt
        )
      )

      val alg = algebra()

      InfixToken.Op.math
        .filterNot(
          // Can not use negative numbers with pow
          _ == InfixToken.Op.Pow && ScalarType.signed(rt)
        )
        .foreach { op =>
          val token = InfixToken[Id](vl, vr, op)

          val (st, res) = alg
            .valueToRaw(token)
            .run(state)
            .value

          inside(res) { case Some(value) =>
            value.`type` shouldBe a[ScalarType]

            if (ut != TopType) {
              value.`type`.acceptsValueOf(lt) shouldBe true
              value.`type`.acceptsValueOf(rt) shouldBe true
            } else {
              // This should happen only if
              // of the types is 64 bit
              List(lt, rt).exists(
                List(ScalarType.u64, ScalarType.i64).contains
              ) shouldBe true

              (value.`type`.acceptsValueOf(lt) ||
                value.`type`.acceptsValueOf(rt)) shouldBe true
            }

          }
        }
    }
  }

  it should "handle * on float literals" in {
    val llit = literal("42.1", LiteralType.float)
    val rlit = literal("37.2", LiteralType.float)

    val alg = algebra()

    val token = InfixToken[Id](llit, rlit, InfixToken.Op.Mul)

    val (st, res) = alg
      .valueToRaw(token)
      .run(genState())
      .value

    inside(res) { case Some(value) =>
      value.`type` shouldBe ScalarType.i64
    }
  }

  it should "handle * on float vars" in {
    allPairs(ScalarType.float.toList).foreach { case (lt, rt) =>
      val lvar = variable("left")
      val rvar = variable("right")

      val alg = algebra()

      val state = genState(
        vars = Map(
          "left" -> lt,
          "right" -> rt
        )
      )

      val token = InfixToken[Id](lvar, rvar, InfixToken.Op.Mul)

      val (st, res) = alg
        .valueToRaw(token)
        .run(state)
        .value

      inside(res) { case Some(value) =>
        value.`type` shouldBe ScalarType.i64
      }
    }
  }

  it should "handle ||, && on bool values" in {
    val types = List(LiteralType.bool, ScalarType.bool)

    allPairs(types).foreach { case (lt, rt) =>
      InfixToken.BoolOp.values.foreach { op =>
        val left = lt match {
          case lt: LiteralType =>
            literal("true", lt)
          case _ =>
            variable("left")
        }
        val right = rt match {
          case rt: LiteralType =>
            literal("false", rt)
          case _ =>
            variable("right")
        }

        val alg = algebra()

        val state = genState(
          vars = (
            List("left" -> lt).filter(_ => lt != LiteralType.bool) ++
              List("right" -> rt).filter(_ => rt != LiteralType.bool)
          ).toMap
        )

        val token = InfixToken[Id](left, right, InfixToken.Op.Bool(op))

        val (st, res) = alg
          .valueToRaw(token)
          .run(state)
          .value

        inside(res) { case Some(ApplyBinaryOpRaw(bop, _, _)) =>
          bop shouldBe (op match {
            case InfixToken.BoolOp.And => ApplyBinaryOpRaw.Op.And
            case InfixToken.BoolOp.Or => ApplyBinaryOpRaw.Op.Or
          })
        }
      }
    }
  }

  it should "handle ==, != on values" in {
    val test = (lt: Type, rt: Type) => {
      InfixToken.EqOp.values.foreach { op =>
        val left = valueOfType(lt)(
          varName = "left",
          bool = "true",
          unsigned = "42",
          signed = "-42",
          string = "\"foo\""
        )
        val right = valueOfType(rt)(
          varName = "right",
          bool = "false",
          unsigned = "37",
          signed = "-37",
          string = "\"bar\""
        )

        val alg = algebra()

        val state = genState(
          vars = (
            List("left" -> lt).filter(_ =>
              lt match {
                case _: LiteralType => false
                case _ => true
              }
            ) ++ List("right" -> rt).filter(_ =>
              rt match
                case _: LiteralType => false
                case _ => true
            )
          ).toMap
        )

        val token = InfixToken[Id](left, right, InfixToken.Op.Eq(op))

        val (st, res) = alg
          .valueToRaw(token)
          .run(state)
          .value

        inside(res) { case Some(ApplyBinaryOpRaw(bop, _, _)) =>
          bop shouldBe (op match {
            case InfixToken.EqOp.Eq => ApplyBinaryOpRaw.Op.Eq
            case InfixToken.EqOp.Neq => ApplyBinaryOpRaw.Op.Neq
          })
        }
      }
    }

    val numbers = ScalarType.integer.toList ++ List(
      LiteralType.signed,
      LiteralType.unsigned
    )

    allPairs(numbers).foreach { case (lt, rt) =>
      test(lt, rt)
    }

    val numberStreams = ScalarType.integer.toList.map(StreamType.apply)

    allPairs(numberStreams).foreach { case (lt, rt) =>
      test(lt, rt)
    }

    val structType = StructType(
      "Struct",
      NonEmptyMap(
        "foo" -> ScalarType.i64,
        SortedMap(
          "bar" -> ScalarType.bool
        )
      )
    )

    test(structType, structType)
  }

  it should "handle ! on bool values" in {
    val types = List(LiteralType.bool, ScalarType.bool)

    types.foreach { t =>
      PrefixToken.Op.values.foreach { op =>
        val value = t match {
          case lt: LiteralType =>
            literal("true", lt)
          case _ =>
            variable("val")
        }

        val alg = algebra()

        val state = genState(
          vars = List("val" -> t).filter(_ => t != LiteralType.bool).toMap
        )

        val token = PrefixToken[Id](value, op)

        val (st, res) = alg
          .valueToRaw(token)
          .run(state)
          .value

        inside(res) { case Some(ApplyUnaryOpRaw(uop, _)) =>
          uop shouldBe (op match {
            case PrefixToken.Op.Not => ApplyUnaryOpRaw.Op.Not
          })
        }
      }
    }
  }

  it should "check type of logical operands (binary)" in {
    val types = List(LiteralType.bool, ScalarType.bool).flatMap(t =>
      List(t -> ScalarType.i8, ScalarType.i8 -> t)
    )

    types.foreach { case (lt, rt) =>
      InfixToken.BoolOp.values.foreach { op =>
        val left = lt match {
          case lt: LiteralType =>
            literal("true", lt)
          case _ =>
            variable("left")
        }
        val right = rt match {
          case rt: LiteralType =>
            literal("false", rt)
          case _ =>
            variable("right")
        }

        val alg = algebra()

        val state = genState(
          vars = (
            List("left" -> lt).filter(_ => lt != LiteralType.bool) ++
              List("right" -> rt).filter(_ => rt != LiteralType.bool)
          ).toMap
        )

        val token = InfixToken[Id](left, right, InfixToken.Op.Bool(op))

        val (st, res) = alg
          .valueToRaw(token)
          .run(state)
          .value

        res shouldBe None
        st.errors.exists(_.isInstanceOf[RulesViolated[Id]]) shouldBe true
      }
    }
  }

  it should "check type of (in)equality operands" in {
    val structType = StructType("Struct", NonEmptyMap.one("field", ScalarType.i8))

    val types =
      List(
        LiteralType.bool,
        ScalarType.i32,
        structType,
        StreamType(ScalarType.i8),
        StreamType(structType),
        ArrowType(
          domain = ProductType(ScalarType.i64 :: Nil),
          codomain = ProductType(ScalarType.bool :: Nil)
        )
      )

    allPairs(types).filterNot { case (lt, rt) => lt == rt }.foreach { case (lt, rt) =>
      InfixToken.EqOp.values.foreach { op =>
        val left = lt match {
          case lt: LiteralType =>
            literal("true", lt)
          case _ =>
            variable("left")
        }
        val right = rt match {
          case rt: LiteralType =>
            literal("false", rt)
          case _ =>
            variable("right")
        }

        val alg = algebra()

        val state = genState(
          vars = (
            List("left" -> lt).filter(_ => lt != LiteralType.bool) ++
              List("right" -> rt).filter(_ => rt != LiteralType.bool)
          ).toMap
        )

        val token = InfixToken[Id](left, right, InfixToken.Op.Eq(op))

        val (st, res) = alg
          .valueToRaw(token)
          .run(state)
          .value

        res shouldBe None
        st.errors.exists(_.isInstanceOf[RulesViolated[Id]]) shouldBe true
      }
    }
  }

  it should "check type of logical operand (unary)" in {
    val types = ScalarType.integer.toList :+ LiteralType.unsigned

    types.foreach { t =>
      PrefixToken.Op.values.foreach { op =>
        val value = t match {
          case lt: LiteralType =>
            literal("42", lt)
          case _ =>
            variable("val")
        }

        val alg = algebra()

        val state = genState(
          vars = Map(
            "value" -> t
          ).filter(_ => t != LiteralType.unsigned)
        )

        val token = PrefixToken[Id](value, op)

        val (st, res) = alg
          .valueToRaw(token)
          .run(state)
          .value

        res shouldBe None
        st.errors.exists(_.isInstanceOf[RulesViolated[Id]]) shouldBe true
      }
    }
  }
}
