package aqua.compiler

import aqua.model.{CallModel, ForModel, FunctorModel, LiteralModel, ValueModel, VarModel}
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

import cats.Id
import cats.data.{Chain, NonEmptyChain, NonEmptyMap, Validated, ValidatedNec}
import cats.instances.string.*
import cats.syntax.show.*
import cats.syntax.option.*
import cats.syntax.either.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.Inside
import aqua.model.AquaContext
import aqua.model.FlattenModel

class AquaCompilerSpec extends AnyFlatSpec with Matchers with Inside {
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

  private def insideContext(
    src: Map[String, String],
    imports: Map[String, String] = Map.empty
  )(
    test: AquaContext => Any
  ) = {
    val compiled = CompilerAPI
      .compileToContext[Id, String, String, Span.S](
        aquaSource(src, imports),
        id => txt => Parser.parse(Parser.parserSchema)(txt),
        AquaCompilerConf(ConstantRaw.defaultConstants(None))
      )
      .value
      .value
      .toValidated

    inside(compiled) { case Validated.Valid(contexts) =>
      inside(contexts.headOption) { case Some(ctx) =>
        test(ctx)
      }
    }
  }

  private def insideRes(
    src: Map[String, String],
    imports: Map[String, String] = Map.empty,
    transformCfg: TransformConfig = TransformConfig()
  )(funcNames: String*)(
    test: PartialFunction[List[FuncRes], Any]
  ) = insideContext(src, imports)(ctx =>
    val aquaRes = Transform.contextRes(ctx, transformCfg)
    // To preserve order as in funcNames do flatMap
    val funcs = funcNames.flatMap(name => aquaRes.funcs.find(_.funcName == name)).toList
    inside(funcs)(test)
  )

  "aqua compiler" should "compile a simple snippet to the right context" in {

    val src = Map(
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
    )

    insideContext(src) { ctx =>
      ctx.allFuncs.contains("foo") should be(true)
      ctx.allFuncs.contains("foo_two") should be(true)

      val const = ctx.allValues.get("X")
      const.nonEmpty should be(true)
      const.get should be(LiteralModel.number(5))
    }
  }

  def through(peer: ValueModel) =
    MakeRes.hop(peer)

  val relay = VarRaw("-relay-", ScalarType.string)

  def getDataSrv(name: String, varName: String, t: Type) = {
    CallServiceRes(
      LiteralModel.fromRaw(LiteralRaw.quote("getDataSrv")),
      name,
      CallRes(Nil, Some(CallModel.Export(varName, t))),
      LiteralModel.fromRaw(ValueRaw.InitPeerId)
    ).leaf
  }

  private val init = LiteralModel.fromRaw(ValueRaw.InitPeerId)

  private def join(vm: VarModel, size: ValueModel) =
    ResBuilder.join(vm, size, init)

  it should "create right topology" in {
    val src = Map(
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
    )

    val transformCfg = TransformConfig()

    insideRes(src, transformCfg = transformCfg)("exec") { case exec :: _ =>
      val peers = VarModel("-peers-arg-", ArrayType(ScalarType.string))
      val peer = VarModel("peer-0", ScalarType.string)
      val resultsType = StreamType(ScalarType.string)
      val results = VarModel("results", resultsType)
      val sizeVar = VarModel("results_size", LiteralType.unsigned) // TODO: Make it u32
      val canonResult =
        VarModel("-" + results.name + "-fix-0", CanonStreamType(resultsType.element))
      val flatResult = VarModel("-results-flat-0", ArrayType(ScalarType.string))
      val initPeer = LiteralModel.fromRaw(ValueRaw.InitPeerId)
      val retVar = VarModel("ret", ScalarType.string)

      val expected =
        XorRes.wrap(
          SeqRes.wrap(
            getDataSrv("-relay-", "-relay-", ScalarType.string),
            getDataSrv("peers", peers.name, peers.`type`),
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
                              Some(CallModel.Export(retVar.name, retVar.`type`))
                            ),
                            peer
                          ).leaf,
                          ApRes(retVar, CallModel.Export(results.name, results.`type`)).leaf,
                          through(ValueModel.fromRaw(relay)),
                          through(initPeer)
                        ),
                        SeqRes.wrap(
                          through(ValueModel.fromRaw(relay)),
                          through(initPeer),
                          failErrorRes
                        )
                      ),
                      NextRes(peer.name).leaf
                    )
                  )
                ),
                ResBuilder.add(
                  LiteralModel.number(2),
                  LiteralModel.number(1),
                  sizeVar,
                  initPeer
                ),
                join(results, sizeVar),
                CanonRes(
                  results,
                  init,
                  CallModel.Export(canonResult.name, canonResult.`type`)
                ).leaf,
                ApRes(
                  canonResult,
                  CallModel.Export(flatResult.name, flatResult.`type`)
                ).leaf
              )
            ),
            respCall(transformCfg, flatResult, initPeer)
          ),
          errorCall(transformCfg, 0, initPeer)
        )

      exec.body.equalsOrShowDiff(expected) shouldBe (true)
    }
  }

  it should "compile with imports" in {

    val src = Map(
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
    )
    val imports = Map(
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

    val transformCfg = TransformConfig(relayVarName = None)

    insideRes(src, imports, transformCfg)(
      "wrap",
      "barfoo"
    ) { case wrap :: barfoo :: _ =>
      val resStreamType = StreamType(ScalarType.string)
      val resVM = VarModel("res", resStreamType)
      val resCanonVM = VarModel("-res-fix-0", CanonStreamType(ScalarType.string))
      val resFlatVM = VarModel("-res-flat-0", ArrayType(ScalarType.string))

      val expected = XorRes.wrap(
        SeqRes.wrap(
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
          respCall(transformCfg, resFlatVM, initPeer)
        ),
        errorCall(transformCfg, 0, initPeer)
      )

      barfoo.body.equalsOrShowDiff(expected) should be(true)
    }
  }

  it should "optimize math inside stream join" in {
    val src = Map(
      "main.aqua" -> """
                       |func main(i: i32):
                       |  stream: *string
                       |  stream <<- "a"
                       |  stream <<- "b"
                       |  join stream[i - 1]
                       |""".stripMargin
    )

    val transformCfg = TransformConfig()

    insideRes(src, transformCfg = transformCfg)("main") { case main :: _ =>
      val streamName = "stream"
      val streamType = StreamType(ScalarType.string)
      val argName = "-i-arg-"
      val argType = ScalarType.i32
      val expected = XorRes.wrap(
        SeqRes.wrap(
          getDataSrv("-relay-", "-relay-", ScalarType.string),
          getDataSrv("i", argName, argType),
          RestrictionRes(streamName, streamType).wrap(
            SeqRes.wrap(
              ApRes(LiteralModel.quote("a"), CallModel.Export(streamName, streamType)).leaf,
              ApRes(LiteralModel.quote("b"), CallModel.Export(streamName, streamType)).leaf,
              join(VarModel(streamName, streamType), VarModel(argName, argType))
            )
          )
        ),
        errorCall(transformCfg, 0, initPeer)
      )

      main.body.equalsOrShowDiff(expected) should be(true)
    }
  }
}
