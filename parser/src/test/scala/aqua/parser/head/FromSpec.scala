package aqua.parser.head

import aqua.AquaSpec
import aqua.parser.expr.func.AbilityIdExpr
import aqua.parser.lexer.{Literal, Token}
import aqua.parser.lift.LiftParser.Implicits.*
import aqua.types.LiteralType
import cats.Id
import cats.data.NonEmptyList
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FromSpec extends AnyFlatSpec with Matchers with AquaSpec {

  import AquaSpec.*

  "from constants" should "be parsed" in {
    FromExpr.nameOrAbAs.parseAll("SOME_CONSTANT").value shouldBe Right(
      (toAb("SOME_CONSTANT"), None)
    )

    FromExpr.nameOrAbAs.parseAll("SOME_CONSTANT as SC").value shouldBe Right(
      (toAb("SOME_CONSTANT"), Some(toAb("SC")))
    )
  }

  "from expression" should "be parsed" in {
    FromExpr.nameOrAbAs.parseAll("Ability").value should be(Right(toAb("Ability") -> None))
    FromExpr.nameOrAbAs.parseAll("Ability as Ab").value should be(
      Right(toAb("Ability") -> Some(toAb("Ab")))
    )
    FromExpr.nameOrAbAs.parseAll("function").value should be(
      Left(toName("function") -> None)
    )
    FromExpr.nameOrAbAs.parseAll("function as fn").value should be(
      Left(toName("function") -> Some(toName("fn")))
    )
  }

  "from list" should "be parsed" in {
    Token.comma(FromExpr.nameOrAbAs).parseAll("Ability").value.head should be(
      Right(toAb("Ability") -> None)
    )
    Token.comma(FromExpr.nameOrAbAs).parseAll("Ability as Ab").value.head should be(
      Right(toAb("Ability") -> Some(toAb("Ab")))
    )

    FromExpr.importFrom.parseAll("Ability as Ab from").value should be(
      NonEmptyList.one(Right(toAb("Ability") -> Some(toAb("Ab"))))
    )
    FromExpr.importFrom.parseAll("Ability from").value should be(
      NonEmptyList.one(Right(toAb("Ability") -> None))
    )
  }

}
