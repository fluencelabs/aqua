package aqua.lsp

import aqua.compiler.{AquaCompilerConf, AquaError, AquaSources}
import aqua.parser.Parser
import aqua.parser.lift.Span
import aqua.parser.lift.Span.S
import aqua.raw.ConstantRaw
import aqua.semantics.rules.locations.{TokenLocation, VariableInfo}
import aqua.types.*
import cats.Id
import cats.data.*
import cats.instances.string.*
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AquaLSPSpec extends AnyFlatSpec with Matchers with Inside {

  extension (c: LspContext[Span.S]) {

    def checkLocations(
      defStart: Int,
      defEnd: Int,
      useStart: Int,
      useEnd: Int
    ): Boolean =
      c.allLocations.exists { case TokenLocation(useT, defT) =>
        val defSpan = defT.unit._1
        val useSpan = useT.unit._1
        defSpan.startIndex == defStart && defSpan.endIndex == defEnd && useSpan.startIndex == useStart && useSpan.endIndex == useEnd
      }

    def checkTokenLoc(
      checkName: String,
      start: Int,
      end: Int,
      `type`: Type
    ): Boolean = {
      val res = c.variables.exists { case VariableInfo(definition, _) =>
        val span = definition.token.unit._1
        definition.name == checkName && span.startIndex == start && span.endIndex == end && definition.`type` == `type`
      }

      /*println(tokens.filter(v => v._1 == checkName && v._2.`type` == `type`).map {
        case (name, expr) =>
          val span = expr.token.unit._1
          println(s"$name(${span.startIndex}:${span.endIndex}) ${expr.`type`}")
      })*/

      res
    }
  }

  private def aquaSource(src: Map[String, String], imports: Map[String, String]) = {
    new AquaSources[Id, String, String] {

      override def sources: Id[ValidatedNec[String, Chain[(String, String)]]] =
        Validated.validNec(Chain.fromSeq(src.toSeq))

      override def resolveImport(from: String, imp: String): Id[ValidatedNec[String, String]] =
        Validated.validNec(imp)

      override def load(file: String): Id[ValidatedNec[String, String]] =
        Validated.fromEither(
          (imports ++ src)
            .get(file)
            .toRight(NonEmptyChain.one(s"Cannot load imported file $file"))
        )
    }
  }

  def compile(
    src: Map[String, String],
    imports: Map[String, String] = Map.empty
  ): ValidatedNec[AquaError[String, String, S], Map[String, LspContext[S]]] = {
    LSPCompiler
      .compileToLsp[Id, String, String, Span.S](
        aquaSource(src, imports),
        id => txt => Parser.parse(Parser.parserSchema)(txt),
        AquaCompilerConf(ConstantRaw.defaultConstants(None))
      )
  }

  it should "return right tokens" in {
    val src = Map(
      "index.aqua" ->
        """module Import
          |import foo, str, num from "export2.aqua"
          |
          |import "../gen/OneMore.aqua"
          |
          |func foo_wrapper() -> string:
          |    z <- foo()
          |    if 1 == 1:
          |      a = "aaa"
          |      str(a)
          |    else:
          |      a = 123
          |      num(a)
          |    OneMore z
          |    OneMore.more_call()
          |""".stripMargin
    )

    val imports = Map(
      "export2.aqua" ->
        """module Export declares str, num, foo
          |
          |func str(a: string) -> string:
          |    <- a
          |
          |func num(a: u32) -> u32:
          |    <- a
          |
          |func foo() -> string:
          |    <- "I am MyFooBar foo"
          |
          |""".stripMargin,
      "../gen/OneMore.aqua" ->
        """
          |service OneMore:
          |  more_call()
          |  consume(s: string)
          |""".stripMargin
    )

    val res = compile(src, imports).toOption.get.values.head

    val serviceType = ServiceType(
      "OneMore",
      NonEmptyMap.of(
        ("more_call", ArrowType(NilType, NilType)),
        ("consume", ArrowType(ProductType.labelled(("s", ScalarType.string) :: Nil), NilType))
      )
    )

    /*println(res.allLocations.map { case TokenLocation(l, r) =>
      val lSpan = l.unit._1
      val rSpan = r.unit._1
      s"($l($lSpan):$r($rSpan))"
    })*/

    // inside `foo_wrapper` func
    res.checkTokenLoc("z", 120, 121, ScalarType.string) shouldBe true
    res.checkLocations(120, 121, 224, 225) shouldBe true

    res.checkTokenLoc("a", 152, 153, LiteralType.string) shouldBe true
    res.checkLocations(152, 153, 172, 173) shouldBe true
    res.checkTokenLoc("a", 191, 192, LiteralType.unsigned) shouldBe true
    res.checkLocations(191, 192, 209, 210) shouldBe true

    // num usage
    res.checkLocations(84, 87, 205, 208) shouldBe true
    // str usage
    res.checkLocations(43, 46, 168, 171) shouldBe true

    // this is tokens from imports, if we will use `FileSpan.F` file names will be different
    // OneMore service
    res.checkTokenLoc("OneMore", 9, 16, serviceType) shouldBe true
    res.checkTokenLoc("OneMore.more_call", 20, 29, ArrowType(NilType, NilType)) shouldBe true
    res.checkTokenLoc(
      "OneMore.consume",
      34,
      41,
      ArrowType(ProductType.labelled(("s", ScalarType.string) :: Nil), NilType)
    ) shouldBe true

    // str function and argument
    res.checkTokenLoc(
      "str",
      43,
      46,
      ArrowType(
        ProductType.labelled(("a", ScalarType.string) :: Nil),
        ProductType(ScalarType.string :: Nil)
      )
    ) shouldBe true
    res.checkTokenLoc("a", 47, 48, ScalarType.string) shouldBe true

    // num function and argument
    res.checkTokenLoc(
      "num",
      84,
      87,
      ArrowType(
        ProductType.labelled(("a", ScalarType.u32) :: Nil),
        ProductType(ScalarType.u32 :: Nil)
      )
    ) shouldBe true
    res.checkTokenLoc("a", 88, 89, ScalarType.u32) shouldBe true

    // foo function
    res.checkTokenLoc(
      "foo",
      119,
      122,
      ArrowType(NilType, ProductType(ScalarType.string :: Nil))
    ) shouldBe true
  }
}
