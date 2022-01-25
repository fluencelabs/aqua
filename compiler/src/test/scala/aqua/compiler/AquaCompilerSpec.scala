package aqua.compiler

import aqua.model.LiteralModel
import aqua.model.transform.TransformConfig
import aqua.parser.ParserError
import aqua.parser.Ast
import aqua.parser.Parser
import aqua.parser.lift.Span
import aqua.types.LiteralType
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import cats.Id
import cats.data.{Chain, NonEmptyChain, Validated, ValidatedNec}
import cats.instances.string.*

class AquaCompilerSpec extends AnyFlatSpec with Matchers {

  private def compileToContext(src: Map[String, String], imports: Map[String, String]) =
    AquaCompiler.compileToContext[Id, String, String, Span.S](
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
      },
      id => txt => Parser.parse(Parser.parserSchema)(txt),
      TransformConfig(wrapWithXor = false)
    )

  "aqua compiler" should "compile a simple snipped to the right context" in {

    val res = compileToContext(
      Map(
        "index.aqua" ->
          """module Foo declares X
            |
            |export foo, foo2 as foo_two, X
            |
            |const X = 5
            |
            |func foo() -> string:
            |  <- "hello?"
            |
            |func foo2() -> string:
            |  <- "hello2?"
            |""".stripMargin
      ),
      Map.empty
    )

    res.isValid should be(true)
    val Validated.Valid(ctxs) = res

    ctxs.length should be(1)
    val ctx = ctxs.headOption.get

    ctx.allFuncs.contains("foo") should be(true)
    ctx.allFuncs.contains("foo_two") should be(true)

    val const = ctx.allValues.get("X")
    const.nonEmpty should be(true)
    const.get should be(LiteralModel("5", LiteralType.number))

  }

  "aqua compiler" should "compile a callback with an empty stream argument" in {

    val res = compileToContext(
      Map(
        "index.aqua" ->
          """module Ret declares *
            |
            |export someFunc
            |
            |func someFunc(cb: []string -> ()):
            |	ifaces: *string
            |	cb(ifaces)
            |""".stripMargin
      ),
      Map.empty
    )

    res.isValid should be(true)
    val Validated.Valid(ctxs) = res

    ctxs.length should be(1)
    val ctx = ctxs.headOption.get

    val func = ctx.allFuncs.get("someFunc")
    func.nonEmpty should be(true)
  }

}
