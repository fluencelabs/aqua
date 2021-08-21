package aqua.parser.head

import aqua.AquaSpec
import aqua.parser.expr.AbilityIdExpr
import aqua.parser.lexer.{Literal, Token}
import aqua.parser.lift.LiftParser.Implicits.*
import aqua.types.LiteralType
import cats.Id
import cats.data.NonEmptyList
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FromSpec extends AnyFlatSpec with Matchers with AquaSpec {

  import AquaSpec.*

  "from expression" should "be parsed" in {
    FromExpr.nameOrAbAs[Id].parseAll("Ability").value should be(Right(toAb("Ability") -> None))
    FromExpr.nameOrAbAs[Id].parseAll("Ability as Ab").value should be(
      Right(toAb("Ability") -> Some(toAb("Ab")))
    )
    FromExpr.nameOrAbAs[Id].parseAll("function").value should be(
      Left(toName("function") -> None)
    )
    FromExpr.nameOrAbAs[Id].parseAll("function as fn").value should be(
      Left(toName("function") -> Some(toName("fn")))
    )
  }

  "from list" should "be parsed" in {
    Token.comma(FromExpr.nameOrAbAs[Id]).parseAll("Ability").value.head should be(
      Right(toAb("Ability") -> None)
    )
    Token.comma(FromExpr.nameOrAbAs[Id]).parseAll("Ability as Ab").value.head should be(
      Right(toAb("Ability") -> Some(toAb("Ab")))
    )

    FromExpr.importFrom[Id].parseAll("Ability as Ab from").value should be(
      NonEmptyList.one(Right(toAb("Ability") -> Some(toAb("Ab"))))
    )
    FromExpr.importFrom[Id].parseAll("Ability from").value should be(
      NonEmptyList.one(Right(toAb("Ability") -> None))
    )
  }

}
