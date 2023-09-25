package aqua.parser.head

import aqua.AquaSpec
import aqua.parser.expr.func.ServiceIdExpr
import aqua.parser.lexer.{LiteralToken, Token}
import aqua.parser.lift.LiftParser.Implicits.*
import aqua.types.LiteralType
import cats.Id
import cats.data.NonEmptyList
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ImportFromSpec extends AnyFlatSpec with Matchers with AquaSpec {
  import AquaSpec.*

  "import from" should "be parsed" in {
    FromExpr.nameOrAbAs.parseAll("")

    ImportFromExpr.p.parseAll("import MyModule from \"file.aqua\"").value.mapK(spanToId) should be(
      ImportFromExpr(
        NonEmptyList.one(Right(toAb("MyModule") -> None)),
        toStr("file.aqua")
      )
    )

    HeadExpr.ast
      .parseAll(s"""import MyModule, func as fn from "file.aqua"
                   |""".stripMargin)
      .value
      .tail
      .value
      .headOption
      .get
      .head
      .mapK(spanToId) should be(
      ImportFromExpr(
        NonEmptyList.fromListUnsafe(
          Right(toAb("MyModule") -> None) :: Left(toName("func") -> Some(toName("fn"))) :: Nil
        ),
        toStr("file.aqua")
      )
    )
  }

}
