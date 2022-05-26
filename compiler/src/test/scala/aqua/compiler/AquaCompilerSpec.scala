package aqua.compiler

import aqua.model.{CallModel, IntoIndexModel, LiteralModel, ValueModel, VarModel}
import aqua.model.transform.TransformConfig
import aqua.model.transform.Transform
import aqua.parser.ParserError
import aqua.parser.Ast
import aqua.parser.Parser
import aqua.parser.lift.Span
import aqua.raw.value.{LiteralRaw, ValueRaw, VarRaw}
import aqua.res.{
  ApRes,
  CallRes,
  CallServiceRes,
  FoldRes,
  MakeRes,
  NextRes,
  ParRes,
  RestrictionRes,
  SeqRes
}
import aqua.types.{ArrayType, LiteralType, ScalarType, StreamType, Type}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import cats.Id
import cats.data.{Chain, NonEmptyChain, Validated, ValidatedNec}
import cats.instances.string.*
import cats.syntax.show.*

class AquaCompilerSpec extends AnyFlatSpec with Matchers {

  private def compileToContext(src: Map[String, String], imports: Map[String, String]) =
    IntermediateCompilation
      .compileToContext[Id, String, String, Span.S](
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
        AquaCompilerConf()
      )
      .map(_._2)

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

  def through(peer: ValueModel, log: String = null) =
    MakeRes.noop(peer, log)

  val relay = VarRaw("-relay-", ScalarType.string)

  def getDataSrv(name: String, t: Type) = {
    CallServiceRes(
      LiteralModel.fromRaw(LiteralRaw.quote("getDataSrv")),
      name,
      CallRes(Nil, Some(CallModel.Export(name, t))),
      LiteralModel.fromRaw(ValueRaw.InitPeerId)
    ).leaf
  }

  "aqua compiler" should "create right topology" in {

    val res = compileToContext(
      Map(
        "index.aqua" ->
          """service Op("op"):
            |  identity(s: string) -> string
            |
            |func exec(peers: []string) -> []string:
            |    results: *string
            |    for peer <- peers par:
            |        on peer:
            |            results <- Op.identity("hahahahah")
            |
            |    join results[2]
            |    <- results""".stripMargin
      ),
      Map.empty
    )

    res.isValid should be(true)
    val Validated.Valid(ctxs) = res

    ctxs.length should be(1)
    val ctx = ctxs.headOption.get

    val aquaRes =
      Transform.contextRes(ctx, TransformConfig(wrapWithXor = false))

    val Some(exec) = aquaRes.funcs.find(_.funcName == "exec")

    val peers = VarModel("peers", ArrayType(ScalarType.string))
    val peer = VarModel("peer-0", ScalarType.string)
    val results = VarModel("results", StreamType(ScalarType.string))
    val initPeer = LiteralModel.fromRaw(ValueRaw.InitPeerId)

    val expected =
      SeqRes.wrap(
        getDataSrv("-relay-", ScalarType.string),
        getDataSrv(peers.name, peers.`type`),
        RestrictionRes("results", true).wrap(
          SeqRes.wrap(
            ParRes.wrap(
              FoldRes(peer.name, peers).wrap(
                ParRes.wrap(
                  // better if first relay will be outside `for`
                  SeqRes.wrap(
                    through(ValueModel.fromRaw(relay)),
                    CallServiceRes(
                      LiteralModel.fromRaw(LiteralRaw.quote("op")),
                      "identity",
                      CallRes(
                        LiteralModel.fromRaw(LiteralRaw.quote("hahahahah")) :: Nil,
                        Some(CallModel.Export(results.name, results.`type`))
                      ),
                      peer
                    ).leaf,
                    through(ValueModel.fromRaw(relay)),
                    through(initPeer)
                  ),
                  NextRes(peer.name).leaf
                )
              )
            ),
            CallServiceRes(
              LiteralModel.fromRaw(LiteralRaw.quote("op")),
              "noop",
              CallRes(
                results.copy(lambda = Chain.one(IntoIndexModel("2", ScalarType.string))) :: Nil,
                None
              ),
              initPeer
            ).leaf,
            CallServiceRes(
              LiteralModel.fromRaw(LiteralRaw.quote("op")),
              "identity",
              CallRes(
                results :: Nil,
                Some(CallModel.Export("results-fix", ArrayType(ScalarType.string)))
              ),
              initPeer
            ).leaf
          )
        ),
        CallServiceRes(
          LiteralModel.fromRaw(LiteralRaw.quote("callbackSrv")),
          "response",
          CallRes(
            VarModel("results-fix", ArrayType(ScalarType.string)) :: Nil,
            None
          ),
          initPeer
        ).leaf
      )

    exec.body.equalsOrShowDiff(expected) shouldBe (true)
  }

  "aqua compiler" should "compile with imports" in {

    val res = compileToContext(
      Map(
        "index.aqua" ->
          """module Import
            |import foobar from "export2.aqua"
            |
            |use foo as f from "export2.aqua" as Exp
            |
            |import "../gen/OneMore.aqua"
            |
            |export foo_wrapper as wrap, foobar as barfoo
            |
            |func foo_wrapper() -> string:
            |    z <- Exp.f()
            |    OneMore "hello"
            |    OneMore.more_call()
            |    -- Exp.f() returns literal, this func must return literal in AIR as well
            |    <- z
            |""".stripMargin
      ),
      Map(
        "export2.aqua" ->
          """module Export declares foobar, foo
            |
            |func bar() -> string:
            |    <- " I am MyFooBar bar"
            |
            |func foo() -> string:
            |    <- "I am MyFooBar foo"
            |
            |func foobar() -> []string:
            |    res: *string
            |    res <- foo()
            |    res <- bar()
            |    <- res
            |
            |""".stripMargin,
        "../gen/OneMore.aqua" ->
          """
            |service OneMore:
            |  more_call()
            |  consume(s: string)
            |""".stripMargin
      )
    )

    res.isValid should be(true)
    val Validated.Valid(ctxs) = res

    ctxs.length should be(1)
    val ctx = ctxs.headOption.get

    val aquaRes =
      Transform.contextRes(ctx, TransformConfig(wrapWithXor = false, relayVarName = None))

    val Some(funcWrap) = aquaRes.funcs.find(_.funcName == "wrap")
    val Some(barfoo) = aquaRes.funcs.find(_.funcName == "barfoo")

    barfoo.body.equalsOrShowDiff(
      SeqRes.wrap(
        RestrictionRes("res", true).wrap(
          SeqRes.wrap(
            // res <- foo()
            ApRes(
              LiteralModel.fromRaw(LiteralRaw.quote("I am MyFooBar foo")),
              CallModel.Export("res", StreamType(ScalarType.string))
            ).leaf,
            // res <- bar()
            ApRes(
              LiteralModel.fromRaw(LiteralRaw.quote(" I am MyFooBar bar")),
              CallModel.Export("res", StreamType(ScalarType.string))
            ).leaf,
            // canonicalization
            CallServiceRes(
              LiteralModel.fromRaw(LiteralRaw.quote("op")),
              "identity",
              CallRes(
                VarModel("res", StreamType(ScalarType.string)) :: Nil,
                Some(CallModel.Export("res-fix", ArrayType(ScalarType.string)))
              ),
              LiteralModel.fromRaw(ValueRaw.InitPeerId)
            ).leaf
          )
        ),
        CallServiceRes(
          LiteralModel.fromRaw(LiteralRaw.quote("callbackSrv")),
          "response",
          CallRes(VarModel("res-fix", ArrayType(ScalarType.string)) :: Nil, None),
          LiteralModel.fromRaw(ValueRaw.InitPeerId)
        ).leaf
      )
    ) should be(true)

  }

}
