package aqua.lsp

import aqua.compiler.FileIdString.given_FileId_String
import aqua.compiler.{AquaCompilerConf, AquaError, AquaSources}
import aqua.parser.Parser
import aqua.parser.lexer.Token
import aqua.parser.lift.Span
import aqua.parser.lift.Span.S
import aqua.raw.ConstantRaw
import aqua.semantics.rules.locations.{DefinitionInfo, TokenLocation, VariableInfo}
import aqua.semantics.{RulesViolated, SemanticError}
import aqua.types.*

import cats.Id
import cats.data.*
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AquaLSPSpec extends AnyFlatSpec with Matchers with Inside {

  private def getByPosition(code: String, str: String, position: Int): Option[(Int, Int)] = {
    str.r.findAllMatchIn(code).toList.lift(position).map(r => (r.start, r.end))
  }

  extension [T](o: Option[T]) {

    def tapNone(f: => Unit): Option[T] =
      o.orElse { f; None }
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
        defPos <- getByPosition(defCode, name, defPosition).tapNone(
          fail(s"Didn't find definition of '$name'")
        )
        usePos <- getByPosition(
          useCode.getOrElse(defCode),
          fieldOrSynonym.getOrElse(name),
          usePosition
        ).tapNone(fail(s"Didn't find usage of '$name'"))
      } yield {
        val (defStart, defEnd) = defPos
        val (useStart, useEnd) = usePos
        c.variables.variables.values.flatten.exists { case VariableInfo(defI, occs) =>
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
        val res =
          c.variables.variables.iterator.flatMap(_._2).exists { case VariableInfo(definition, _) =>
            val span = definition.token.unit._1
            definition.name.value == fullName.getOrElse(
              checkName
            ) && span.startIndex == start && span.endIndex == end && definition.`type` == `type`
          }

        if (printFiltered)
          println(
            c.variables.definitions
              .filter(v => v.name.value == fullName.getOrElse(checkName))
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

  def insideError(err: SemanticError[S], str: String, pos: Int, code: String) = {
    inside(err) { case RulesViolated(token, _) =>
      val span = token.unit._1
      val locatedOp = getByPosition(code, str, pos)
      locatedOp shouldBe defined
      val located = locatedOp.get
      (span.startIndex, span.endIndex) shouldBe (located._1, located._2)
    }
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
        |    <- "123"
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

  it should "resolve type tokens in one file correctly" in {
    val main =
      """
        |aqua A declares withAb
        |
        |export main
        |
        |alias SomeAlias: string
        |
        |data NestedStruct:
        |  a: SomeAlias
        |
        |data SomeStruct:
        |  al11: SomeAlias
        |  nested: NestedStruct
        |
        |ability SomeAbility:
        |  someStr: SomeStruct
        |  nested: NestedStruct
        |  al11: SomeAlias
        |  someFunc(ss: SomeStruct, nest: NestedStruct, al11: SomeAlias) -> NestedStruct, SomeStruct, SomeAlias
        |
        |service Srv("a"):
        |  check(ss: SomeStruct, nest: NestedStruct, al11: SomeAlias) -> NestedStruct
        |  check2() -> SomeStruct
        |  check3() -> SomeAlias
        |
        |func withAb{SomeAbility}() -> SomeStruct:
        |  Srv.check(SomeAbility.someStr, SomeAbility.nested, SomeAbility.al11)
        |  Srv.check2()
        |  <- SomeAbility.someStr
        |
        |func main(ss: SomeStruct, nest: NestedStruct, al11: SomeAlias) -> string:
        |  Srv.check3()
        |  <- ""
        |""".stripMargin

    val src = Map(
      "index.aqua" -> main
    )

    val res = compile(src, Map.empty).toOption.get.values.head

    val nestedType = StructType("NestedStruct", NonEmptyMap.of(("a", ScalarType.string)))
    val someStr =
      StructType("SomeStruct", NonEmptyMap.of(("nested", nestedType), ("al11", ScalarType.string)))

    val abFuncType = ArrowType(
      ProductType.labelled(
        ("ss", someStr) :: ("nest", nestedType) :: ("al11", ScalarType.string) :: Nil
      ),
      ProductType(nestedType :: someStr :: ScalarType.string :: Nil)
    )
    val someAb = AbilityType(
      "SomeAbility",
      NonEmptyMap.of(
        ("someStr", someStr),
        ("nested", nestedType),
        ("al11", ScalarType.string),
        ("someFunc", abFuncType)
      )
    )

    val srvType = ServiceType(
      "Srv",
      NonEmptyMap.of(
        (
          "check",
          ArrowType(
            ProductType.labelled(
              ("ss", someStr) :: ("nest", nestedType) :: ("al11", ScalarType.string) :: Nil
            ),
            ProductType(nestedType :: Nil)
          )
        ),
        ("check2", ArrowType(NilType, ProductType(someStr :: Nil))),
        ("check3", ArrowType(NilType, ProductType(ScalarType.string :: Nil)))
      )
    )

    res.checkTokenLoc(main, "SomeAlias", 0, ScalarType.string) shouldBe true
    Range.inclusive(1, 8).foreach { n =>
      res.checkLocations("SomeAlias", 0, n, main) shouldBe true
    }

    res.checkTokenLoc(main, "NestedStruct", 0, nestedType) shouldBe true
    Range.inclusive(1, 7).foreach { n =>
      res.checkLocations("NestedStruct", 0, n, main) shouldBe true
    }

    res.checkTokenLoc(main, "SomeStruct", 0, someStr) shouldBe true
    Range.inclusive(1, 7).foreach { n =>
      res.checkLocations("SomeStruct", 0, n, main) shouldBe true
    }

    res.checkTokenLoc(main, "SomeAbility", 0, someAb) shouldBe true
    Range.inclusive(1, 5).foreach { n =>
      res.checkLocations("SomeAbility", 0, n, main) shouldBe true
    }

    res.checkLocations("someStr", 0, 1, main) shouldBe true
    res.checkLocations("someStr", 0, 2, main) shouldBe true
    res.checkLocations("nested", 1, 2, main) shouldBe true
    res.checkLocations("al11", 1, 4, main) shouldBe true

    res.checkTokenLoc(main, "Srv", 0, srvType) shouldBe true
    Range.inclusive(1, 3).foreach { n =>
      res.checkLocations("Srv", 0, n, main) shouldBe true
    }
  }

  it should "return right tokens with no errors when using different file" in {
    val main =
      """aqua Import declares *
        |
        |use timeout, someString from "export2.aqua" as Export
        |import someString from "export2.aqua"
        |
        |export timeout
        |
        |func timeout() -> string:
        |  res <- Export.timeout()
        |  b = someString()
        |  a = Export.someString()
        |  <- res
        |
        |""".stripMargin
    val src = Map(
      "index.aqua" -> main
    )

    val firstImport =
      """aqua B declares timeout, someString
        |
        |func timeout() -> string:
        |  <- "hack file"
        |
        |func someString() -> string:
        |  <- "sssss"
        |
        |""".stripMargin

    val imports = Map(
      "export2.aqua" ->
        firstImport
    )

    val res = compile(src, imports).toOption.get.values.head

    res.errors shouldBe empty
    res.checkLocations("timeout", 1, 0, firstImport, Some(main)) shouldBe true
    res.checkLocations("timeout", 1, 1, firstImport, Some(main)) shouldBe false
    res.checkLocations("timeout", 1, 2, firstImport, Some(main)) shouldBe false
    res.checkLocations("timeout", 1, 3, firstImport, Some(main)) shouldBe true
    res.checkLocations("someString", 1, 0, firstImport, Some(main)) shouldBe true
    res.checkLocations("someString", 1, 2, firstImport, Some(main)) shouldBe true
    res.checkLocations("someString", 1, 3, firstImport, Some(main)) shouldBe true
  }

  it should "return right tokens in 'use'd structures" in {
    val main =
      """aqua Job declares *
        |
        |use "declare.aqua"
        |
        |export timeout
        |
        |func timeout() -> string:
        |  w <- AquaName.getWorker()
        |  a = w.host_id
        |  ab = AquaName.SomeAbility(getWrk = AquaName.getWorker, someField = "123")
        |  c = ab.getWrk()
        |  d = ab.someField
        |  <- a
        |""".stripMargin
    val src = Map(
      "index.aqua" -> main
    )

    val firstImport =
      """aqua AquaName declares getWorker, Worker, SomeAbility
        |
        |data Worker:
        |  host_id: string
        |
        |ability SomeAbility:
        |  getWrk() -> Worker
        |  someField: string
        |
        |func getWorker() -> Worker:
        |  <- Worker(host_id = "")
        |
        |""".stripMargin

    val imports = Map(
      "declare.aqua" ->
        firstImport
    )

    val res = compile(src, imports).toOption.get.values.head

    res.errors shouldBe empty
    res.checkLocations("host_id", 0, 0, firstImport, Some(main)) shouldBe true
    res.checkLocations("getWorker", 1, 0, firstImport, Some(main)) shouldBe true
    res.checkLocations("getWorker", 1, 0, firstImport) shouldBe true
    res.checkLocations("getWrk", 0, 1, firstImport, Some(main)) shouldBe true
    res.checkLocations("someField", 0, 1, firstImport, Some(main)) shouldBe true
  }

  it should "return right tokens for multiple abilities" in {
    val main =
      """aqua Import declares *
        |
        |export main
        |
        |ability Abilyy:
        |  field: string
        |
        |func firstFunc{Abilyy}() -> string:
        |  <- "str"
        |
        |func secondFunc{Abilyy}() -> string:
        |  <- "str"
        |
        |func main() -> string:
        |  ab = Abilyy(field = "123")
        |  res <- firstFunc{ab}()
        |  secondFunc{ab}()
        |  <- res
        |""".stripMargin
    val src = Map(
      "index.aqua" -> main
    )

    val imports = Map.empty[String, String]

    val res = compile(src, imports).toOption.get.values.head

    res.errors shouldBe empty
    res.checkLocations("Abilyy", 0, 1, main) shouldBe true
    res.checkLocations("Abilyy", 0, 2, main) shouldBe true
    res.checkLocations("Abilyy", 0, 3, main) shouldBe true
  }

  it should "return correct locations of errors on exported functions (LNG-356)" in {
    val main =
      """aqua Job declares *
        |
        |export aaa
        |
        |data Peer:
        |  id: string
        |
        |func aaa() -> string:
        |  peer = Pe2er(id = "123")
        |  <- peer.id""".stripMargin
    val src = Map(
      "index.aqua" -> main
    )

    val imports = Map.empty[String, String]

    val res = compile(src, imports).toEither.toOption.get.values.head

    val errors = res.errors

    insideError(errors.head, "Pe2er", 0, main)
    insideError(errors(1), "peer", 1, main)
    insideError(errors(2), "string", 1, main)
  }

  it should "return correct locations in functions even if there is errors in other parts of a code" in {
    val main =
      """aqua Job declares *
        |
        |export aaa, bbb
        |
        |data Peer:
        |  id: string
        |
        |func aaa() -> Peer:
        |  peer1 = Peer(id = "123")
        |  peer2 = Peer(id = peer1.id)
        |  <- peer2
        |
        |data BrokenStruct:
        |  fff: UnknownType1
        |
        |alias BrokenAlias: UnknownType2
        |
        |ability BrokenAbility:
        |  fff: UnknownType3
        |
        |const BROKEN_CONST = UNKNOWN_CONST
        |
        |func bbb() -> string:
        |  <- 323
        |
        |func ccc() -> Peer:
        |  peer1 = Peer(id = "123")
        |  peer2 = Peer(id = peer1.id)
        |  <- peer2
        |
        |""".stripMargin
    val src = Map(
      "index.aqua" -> main
    )

    val imports = Map.empty[String, String]

    val res = compile(src, imports).toOption.get.values.head
    val errors = res.errors

    // 'aaa' function
    res.checkLocations("Peer", 0, 1, main) shouldBe true
    res.checkLocations("Peer", 0, 2, main) shouldBe true
    res.checkLocations("Peer", 0, 3, main) shouldBe true
    res.checkLocations("peer1", 0, 1, main) shouldBe true
    res.checkLocations("peer1", 0, 1, main) shouldBe true

    // 'ccc' function
    res.checkLocations("Peer", 0, 4, main) shouldBe true
    res.checkLocations("Peer", 0, 5, main) shouldBe true
    res.checkLocations("Peer", 0, 6, main) shouldBe true
    res.checkLocations("peer1", 2, 3, main) shouldBe true
    res.checkLocations("peer1", 2, 3, main) shouldBe true

    // errors
    insideError(errors.head, "UnknownType1", 0, main)
    insideError(errors(1), "BrokenStruct", 0, main)
    insideError(errors(2), "UnknownType2", 0, main)
    insideError(errors(3), "UnknownType3", 0, main)
    insideError(errors(4), "BrokenAbility", 0, main)
    insideError(errors(5), "UNKNOWN_CONST", 0, main)
    insideError(errors(6), "323", 0, main)
    insideError(errors(7), "string", 1, main)
  }

}
