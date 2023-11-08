package aqua.semantics

import aqua.parser.lexer.*
import aqua.semantics.rules.types.{TypeResolution, TypesState}
import aqua.types.*

import cats.Endo
import cats.Id
import cats.SemigroupK
import cats.data.NonEmptyMap
import cats.data.Validated.*
import cats.kernel.Semigroup
import cats.syntax.foldable.*
import cats.syntax.option.*
import cats.syntax.semigroup.*
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TypeResolutionSpec extends AnyFlatSpec with Matchers with Inside {

  given [A, B]: Semigroup[(Endo[A], Endo[B])] with {
    private val algebraA = SemigroupK[Endo].algebra[A]
    private val algebraB = SemigroupK[Endo].algebra[B]

    override def combine(x: (Endo[A], Endo[B]), y: (Endo[A], Endo[B])): (Endo[A], Endo[B]) =
      (algebraA.combine(x._1, y._1), algebraB.combine(x._2, y._2))
  }

  def btt(st: ScalarType): BasicTypeToken[Id] = BasicTypeToken(st)

  def ntt(name: String): NamedTypeToken[Id] = NamedTypeToken(name)

  def resolve(
    token: TypeToken[Id],
    types: Map[String, Type]
  ): TypeResolution.Res[Id, Type] =
    TypeResolution.resolveTypeToken(token)(TypesState(strict = types))

  "TypeResolution resolveTypeToken" should "resolve basic types" in {
    val structType = StructType("Struct", NonEmptyMap.of("field" -> ScalarType.i8))

    val baseTypes = List(
      btt(ScalarType.u32) -> ScalarType.u32,
      btt(ScalarType.string) -> ScalarType.string,
      ntt("Struct") -> structType
    )

    for {
      base <- baseTypes
      (token, expected) = base
    } inside(resolve(token, Map("Struct" -> structType))) {
      case Valid(TypeResolution(result, Nil)) =>
        result shouldEqual expected
    }
  }

  it should "resolve nested types" in {
    val structType = StructType("Struct", NonEmptyMap.of("field" -> ScalarType.i8))

    val baseTypes = List(
      btt(ScalarType.u32) -> ScalarType.u32,
      btt(ScalarType.string) -> ScalarType.string,
      ntt("Struct") -> structType
    )

    val baseModifiers: List[(Endo[DataTypeToken[Id]], Endo[DataType])] = List(
      (ArrayTypeToken[Id]((), _)) -> (ArrayType.apply),
      (OptionTypeToken[Id]((), _)) -> (OptionType.apply)
    )

    val streamModifier = (dt: DataTypeToken[Id]) => StreamTypeToken[Id]((), dt)

    val dataModifiers = LazyList.unfold(baseModifiers) { mods =>
      (
        mods,
        for {
          m <- mods
          b <- baseModifiers
        } yield m combine b
      ).some
    }

    val modifiers = List((streamModifier, StreamType.apply)) +:
      dataModifiers.map { mods =>
        mods.map { case (token, typ) =>
          (token andThen streamModifier) -> (typ andThen StreamType.apply)
        } ++ mods
      }.take(6).toList

    modifiers.foreach(mods =>
      for {
        base <- baseTypes
        (btoken, btype) = base
        modifier <- mods
        (mod, typ) = modifier
      } inside(resolve(mod(btoken), Map("Struct" -> structType))) {
        case Valid(TypeResolution(result, Nil)) =>
          result shouldEqual typ(btype)
      }
    )

  }
}
