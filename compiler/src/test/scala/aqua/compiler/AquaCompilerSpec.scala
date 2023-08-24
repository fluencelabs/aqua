package aqua.compiler

import aqua.model.{
  CallModel,
  ForModel,
  FunctorModel,
  IntoIndexModel,
  LiteralModel,
  ValueModel,
  VarModel
}
import aqua.model.transform.ModelBuilder
import aqua.model.transform.TransformConfig
import aqua.model.transform.Transform
import aqua.parser.ParserError
import aqua.parser.Ast
import aqua.parser.Parser
import aqua.parser.lift.Span
import aqua.parser.lift.Span.S
import aqua.raw.ConstantRaw
import aqua.raw.value.{LiteralRaw, ValueRaw, VarRaw}
import aqua.res.*
import aqua.res.ResBuilder
import aqua.types.{ArrayType, CanonStreamType, LiteralType, ScalarType, StreamType, Type}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import cats.Id
import cats.data.{Chain, NonEmptyChain, NonEmptyMap, Validated, ValidatedNec}
import cats.instances.string.*
import cats.syntax.show.*
import cats.syntax.option.*

class AquaCompilerSpec extends AnyFlatSpec with Matchers {
  import ModelBuilder.*

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

  private def compileToContext(src: Map[String, String], imports: Map[String, String]) =
    CompilerAPI
      .compileToContext[Id, String, String, Span.S](
        aquaSource(src, imports),
        id => txt => Parser.parse(Parser.parserSchema)(txt),
        AquaCompilerConf(ConstantRaw.defaultConstants(None))
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

    println(res)

    res.isValid should be(true)
    val Validated.Valid(ctxs) = res

    ctxs.length should be(1)
    val ctx = ctxs.headOption.get

    ctx.allFuncs.contains("foo") should be(true)
    ctx.allFuncs.contains("foo_two") should be(true)

    val const = ctx.allValues.get("X")
    const.nonEmpty should be(true)
    const.get should be(LiteralModel.number(5))

  }

  def through(peer: ValueModel) =
    MakeRes.hop(peer)

  val relay = VarRaw("-relay-", ScalarType.string)

  def getDataSrv(name: String, t: Type) = {
    CallServiceRes(
      LiteralModel.fromRaw(LiteralRaw.quote("getDataSrv")),
      name,
      CallRes(Nil, Some(CallModel.Export(name, t))),
      LiteralModel.fromRaw(ValueRaw.InitPeerId)
    ).leaf
  }

  private val init = LiteralModel.fromRaw(ValueRaw.InitPeerId)

  private def join(vm: VarModel, idx: ValueModel) =
    ResBuilder.join(vm, idx, init)

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

    val transformCfg = TransformConfig()
    val aquaRes = Transform.contextRes(ctx, transformCfg)

    val Some(exec) = aquaRes.funcs.find(_.funcName == "exec")

    val peers = VarModel("peers", ArrayType(ScalarType.string))
    val peer = VarModel("peer-0", ScalarType.string)
    val resultsType = StreamType(ScalarType.string)
    val results = VarModel("results", resultsType)
    val canonResult = VarModel("-" + results.name + "-fix-0", CanonStreamType(resultsType.element))
    val flatResult = VarModel("-results-flat-0", ArrayType(ScalarType.string))
    val initPeer = LiteralModel.fromRaw(ValueRaw.InitPeerId)

    val expected =
      SeqRes.wrap(
        getDataSrv("-relay-", ScalarType.string),
        getDataSrv(peers.name, peers.`type`),
        XorRes.wrap(
          RestrictionRes(results.name, resultsType).wrap(
            SeqRes.wrap(
              ParRes.wrap(
                FoldRes(peer.name, peers, ForModel.Mode.Never.some).wrap(
                  ParRes.wrap(
                    XorRes.wrap(
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
                      SeqRes.wrap(
                        through(ValueModel.fromRaw(relay)),
                        through(initPeer),
                        failLastErrorRes
                      )
                    ),
                    NextRes(peer.name).leaf
                  )
                )
              ),
              join(results, LiteralModel.fromRaw(LiteralRaw.number(2))),
              CanonRes(results, init, CallModel.Export(canonResult.name, canonResult.`type`)).leaf,
              ApRes(
                canonResult,
                CallModel.Export(flatResult.name, flatResult.`type`)
              ).leaf
            )
          ),
          errorCall(transformCfg, 0, initPeer)
        ),
        respCall(transformCfg, flatResult, initPeer)
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

    val transformCfg = TransformConfig(relayVarName = None)
    val aquaRes = Transform.contextRes(ctx, transformCfg)

    val Some(funcWrap) = aquaRes.funcs.find(_.funcName == "wrap")
    val Some(barfoo) = aquaRes.funcs.find(_.funcName == "barfoo")

    val resStreamType = StreamType(ScalarType.string)
    val resVM = VarModel("res", resStreamType)
    val resCanonVM = VarModel("-res-fix-0", CanonStreamType(ScalarType.string))
    val resFlatVM = VarModel("-res-flat-0", ArrayType(ScalarType.string))

    val expected = SeqRes.wrap(
      XorRes.wrap(
        RestrictionRes(resVM.name, resStreamType).wrap(
          SeqRes.wrap(
            // res <- foo()
            ApRes(
              LiteralModel.fromRaw(LiteralRaw.quote("I am MyFooBar foo")),
              CallModel.Export(resVM.name, resVM.`type`)
            ).leaf,
            // res <- bar()
            ApRes(
              LiteralModel.fromRaw(LiteralRaw.quote(" I am MyFooBar bar")),
              CallModel.Export(resVM.name, resVM.`type`)
            ).leaf,
            // canonicalization
            CanonRes(
              resVM,
              LiteralModel.fromRaw(ValueRaw.InitPeerId),
              CallModel.Export(resCanonVM.name, resCanonVM.`type`)
            ).leaf,
            // flattening
            ApRes(
              VarModel(resCanonVM.name, resCanonVM.`type`),
              CallModel.Export(resFlatVM.name, resFlatVM.`type`)
            ).leaf
          )
        ),
        errorCall(transformCfg, 0, initPeer)
      ),
      respCall(transformCfg, resFlatVM, initPeer)
    )

    barfoo.body.equalsOrShowDiff(expected) should be(true)

  }
}
