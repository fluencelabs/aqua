package aqua.lsp

import aqua.compiler.{AquaCompilerConf, AquaError, AquaSources}
import aqua.parser.Parser
import aqua.parser.lift.Span
import aqua.parser.lift.Span.S
import aqua.raw.ConstantRaw
import aqua.semantics.rules.locations.{DefinitionInfo, TokenLocation, VariableInfo}
import aqua.types.*

import cats.Id
import cats.data.*
import cats.instances.string.*
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AquaLSPSpec extends AnyFlatSpec with Matchers with Inside {

  private def getByPosition(code: String, str: String, position: Int): Option[(Int, Int)] = {
    str.r.findAllMatchIn(code).toList.lift(position).map(r => (r.start, r.end))
  }

  extension (c: LspContext[Span.S]) {

    def checkLocations(
      name: String,
      defPosition: Int,
      usePosition: Int,
      defCode: String,
      useCode: Option[String] = None,
      fieldOrSynonym: Option[String] = None
    ): Boolean = {
      (for {
        defPos <- getByPosition(defCode, name, defPosition)
        usePos <- getByPosition(
          useCode.getOrElse(defCode),
          fieldOrSynonym.getOrElse(name),
          usePosition
        )
      } yield {
        val (defStart, defEnd) = defPos
        val (useStart, useEnd) = usePos
        c.variables.exists { case VariableInfo(defI, occs) =>
          val defSpan = defI.token.unit._1
          if (defSpan.startIndex == defStart && defSpan.endIndex == defEnd) {
            occs.exists { useT =>
              val useSpan = useT.unit._1
              useSpan.startIndex == useStart && useSpan.endIndex == useEnd
            }
          } else {
            false
          }

        }
      }).getOrElse(false)
    }

    def locationsToString(): List[String] =
      c.allLocations.map { case TokenLocation(l, r) =>
        val lSpan = l.unit._1
        val rSpan = r.unit._1
        s"($l($lSpan):$r($rSpan))"
      }

    def checkTokenLoc(
      code: String,
      checkName: String,
      position: Int,
      `type`: Type,
      // if name is combined
      fullName: Option[String] = None,
      printFiltered: Boolean = false
    ): Boolean = {

      getByPosition(code, checkName, position).exists { case (start, end) =>
        val res = c.variables.exists { case VariableInfo(definition, _) =>
          val span = definition.token.unit._1
          definition.name == fullName.getOrElse(
            checkName
          ) && span.startIndex == start && span.endIndex == end && definition.`type` == `type`
        }

        if (printFiltered)
          println(
            c.variables
              .map(_.definition)
              .filter(v => v.name == fullName.getOrElse(checkName) && v.`type` == `type`)
              .map { case DefinitionInfo(name, token, t) =>
                val span = token.unit._1
                s"$name(${span.startIndex}:${span.endIndex}) $t"
              }
          )

        res
      }

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
  ): ValidatedNec[AquaError[String, String, S], Map[String, LspContext[S]]] =
    LSPCompiler
      .compileToLsp[Id, String, String, Span.S](
        aquaSource(src, imports),
        id => txt => Parser.parse(Parser.parserSchema)(txt),
        AquaCompilerConf(ConstantRaw.defaultConstants(None))
      )
      .leftMap { ee =>
        println(ee)
        ee
      }

  it should "return right tokens" in {
    val main =
      """aqua Import declares foo_wrapper, Ab, Str, useAbAndStruct, SOME_CONST
        |
        |import foo, strFunc, num, absb as otherName from "export2.aqua"
        |
        |use thirdFunc as thirdRenamed from "third.aqua" as Third
        |
        |import "../gen/OneMore.aqua"
        |
        |export foo_wrapper, SOME_CONST, EXPORTED as NEW_NAME
        |
        |func foo_wrapper() -> string:
        |    fooResult <- foo()
        |    if 1 == 1:
        |      someVar = "aaa"
        |      strFunc(someVar)
        |    else:
        |      someVar = 123
        |      num(someVar)
        |    OneMore fooResult
        |    OneMore.more_call()
        |
        |ability Ab:
        |    someField: u32
        |
        |data Str:
        |   someField: string
        |
        |func useAbAndStruct{Ab}():
        |    s = Str(someField = "asd")
        |    strFunc(s.someField)
        |    num(Ab.someField)
        |
        |const SOME_CONST = 1
        |const EXPORTED = 1
        |
        |""".stripMargin
    val src = Map(
      "index.aqua" -> main
    )

    val firstImport =
      """aqua Export declares strFunc, num, foo, absb
        |
        |func absb() -> string:
        |    <- "ff"
        |
        |func strFunc(someVar: string) -> string:
        |    <- someVar
        |
        |func num(someVar: u32) -> u32:
        |    <- someVar
        |
        |func foo() -> string:
        |    <- "I am MyFooBar foo"
        |
        |""".stripMargin

    val secondImport =
      """aqua Export declares OneMore
        |
        |service OneMore:
        |  more_call()
        |  consume(s: string)
        |""".stripMargin

    val thirdImport =
      """aqua Third declares thirdFunc
        |
        |func thirdFunc() -> string:
        |    <- "I am MyFooBar foo"
        |
        |""".stripMargin

    val imports = Map(
      "export2.aqua" ->
        firstImport,
      "../gen/OneMore.aqua" ->
        secondImport,
      "third.aqua" ->
        thirdImport
    )

    val res = compile(src, imports).toOption.get.values.head

    val serviceType = ServiceType(
      "OneMore",
      NonEmptyMap.of(
        ("more_call", ArrowType(NilType, NilType)),
        ("consume", ArrowType(ProductType.labelled(("s", ScalarType.string) :: Nil), NilType))
      )
    )

    res.checkTokenLoc(
      main,
      "foo_wrapper",
      2,
      ArrowType(
        NilType,
        ProductType(ScalarType.string :: Nil)
      )
    ) shouldBe true
    res.checkTokenLoc(
      main,
      "SOME_CONST",
      2,
      LiteralType.unsigned
    ) shouldBe true

    // exports
    res.checkLocations("foo_wrapper", 2, 1, main) shouldBe true
    res.checkLocations("SOME_CONST", 2, 1, main) shouldBe true
    res.checkLocations("EXPORTED", 1, 0, main) shouldBe true
    res.checkLocations("EXPORTED", 1, 0, main, None, Some("NEW_NAME")) shouldBe true

    // declares
    res.checkLocations("foo_wrapper", 2, 0, main) shouldBe true
    res.checkLocations("SOME_CONST", 2, 0, main) shouldBe true

    // imports
    res.checkLocations("foo", 1, 1, firstImport, Some(main)) shouldBe true
    res.checkLocations("strFunc", 1, 0, firstImport, Some(main)) shouldBe true
    res.checkLocations("num", 1, 0, firstImport, Some(main)) shouldBe true
    res.checkLocations("absb", 1, 0, firstImport, Some(main)) shouldBe true
    res.checkLocations("absb", 1, 0, firstImport, Some(main), Some("otherName")) shouldBe true

    // use
    res.checkLocations("thirdFunc", 1, 0, thirdImport, Some(main)) shouldBe true
    res.checkLocations(
      "thirdFunc",
      1,
      0,
      thirdImport,
      Some(main),
      Some("thirdRenamed")
    ) shouldBe true

    // inside `foo_wrapper` func
    res.checkTokenLoc(main, "fooResult", 0, ScalarType.string) shouldBe true
    res.checkLocations("fooResult", 0, 1, main) shouldBe true

    res.checkTokenLoc(main, "someVar", 0, LiteralType.string, None) shouldBe true
    res.checkLocations("someVar", 0, 1, main) shouldBe true
    res.checkTokenLoc(main, "someVar", 2, LiteralType.unsigned) shouldBe true
    res.checkLocations("someVar", 2, 3, main) shouldBe true

    // num usage
    res.checkLocations("num", 1, 1, firstImport, Some(main)) shouldBe true
    // strFunc usage
    res.checkLocations("strFunc", 1, 1, firstImport, Some(main)) shouldBe true
    res.checkLocations("strFunc", 1, 2, firstImport, Some(main)) shouldBe true

    // Str.field
    res.checkTokenLoc(main, "someField", 1, ScalarType.string, Some("Str.someField")) shouldBe true
    res.checkLocations("someField", 1, 3, main, None) shouldBe true

    // Ab.field
    res.checkTokenLoc(
      main,
      "someField",
      0,
      ScalarType.u32,
      Some("Ab.someField")
    ) shouldBe true

    // this is tokens from imports, if we will use `FileSpan.F` file names will be different
    // OneMore service
    res.checkTokenLoc(secondImport, "OneMore", 1, serviceType) shouldBe true
    res.checkTokenLoc(
      secondImport,
      "more_call",
      0,
      ArrowType(NilType, NilType),
      Some("OneMore.more_call")
    ) shouldBe true
    res.checkTokenLoc(
      secondImport,
      "consume",
      0,
      ArrowType(ProductType.labelled(("s", ScalarType.string) :: Nil), NilType),
      Some("OneMore.consume")
    ) shouldBe true

    // strFunc function and argument
    res.checkTokenLoc(
      firstImport,
      "strFunc",
      1,
      ArrowType(
        ProductType.labelled(("someVar", ScalarType.string) :: Nil),
        ProductType(ScalarType.string :: Nil)
      ),
      None
    ) shouldBe true
    res.checkTokenLoc(firstImport, "someVar", 0, ScalarType.string) shouldBe true

    // num function and argument
    res.checkTokenLoc(
      firstImport,
      "num",
      1,
      ArrowType(
        ProductType.labelled(("someVar", ScalarType.u32) :: Nil),
        ProductType(ScalarType.u32 :: Nil)
      )
    ) shouldBe true
    res.checkTokenLoc(firstImport, "someVar", 2, ScalarType.u32, None) shouldBe true

    // foo function
    res.checkTokenLoc(
      firstImport,
      "foo",
      1,
      ArrowType(NilType, ProductType(ScalarType.string :: Nil))
    ) shouldBe true
  }
}
