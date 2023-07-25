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
import aqua.raw.value.LiteralRaw
import aqua.raw.RawContext
import aqua.types.{LiteralType, ScalarType, TopType, Type}
import aqua.parser.lexer.{InfixToken, LiteralToken, Name, ValueToken, VarToken}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.Inside
import cats.Id
import cats.data.State
import cats.syntax.functor.*
import cats.syntax.comonad.*
import monocle.syntax.all.*

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

  def variable(name: String) =
    VarToken(Name(Id(name)), Nil)

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
}
