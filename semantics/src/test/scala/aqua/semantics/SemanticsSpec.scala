package aqua.semantics

//import aqua.AquaSpec
//import aqua.model.transform._
//import aqua.model.{AquaContext, Node, VarModel}
//import aqua.parser.Ast
//import aqua.parser.lift.{LiftParser, Span}
//import aqua.types.ScalarType
//import cats.data.Chain
//import org.scalatest.flatspec.AnyFlatSpec
//import org.scalatest.matchers.should.Matchers
//
//class SemanticsSpec extends AnyFlatSpec with Matchers with AquaSpec {
//
//  // use it to fix https://github.com/fluencelabs/aqua/issues/90
//  "sem" should "create right model" in {
//    implicit val fileLift: LiftParser[Span.F] = Span.spanLiftParser
//
//    val script =
//      """service CustomId("cid"):
//        |  id(s: string) -> string
//        |  ids() -> string
//        |
//        |func viaArr(node_id: string, viaAr: []string) -> string:
//        |    on node_id via viaAr:
//        |        p <- CustomId.ids()
//        |    <- p""".stripMargin
//
//    val ast = Ast.fromString(script).toList.head
//
//    val ctx = AquaContext.blank
//    val bc = BodyConfig()
//    import bc.aquaContextMonoid
//
//    val func = Semantics.process(ast, ctx).toList.head.funcs("viaArr")
//
//    val initCallable: InitPeerCallable = InitViaRelayCallable(
//      Chain.fromOption(bc.relayVarName).map(VarModel(_, ScalarType.string))
//    )
//
//    val argsProvider: ArgsProvider =
//      ArgsFromService(
//        bc.dataSrvId,
//        bc.relayVarName.map(_ -> ScalarType.string).toList ::: func.args.dataArgs.toList.map(add =>
//          add.name -> add.dataType
//        )
//      )
//
//    val transform =
//      initCallable.transform _ compose argsProvider.transform
//
//    val callback = initCallable.service(bc.callbackSrvId)
//
//    val wrapFunc = ResolveFunc(
//      transform,
//      callback,
//      bc.respFuncName
//    )
//
//    val tree =
//      wrapFunc.resolve(func).value.tree
//
//    println(Node.cofToNode(tree))
//
//    // SO
////    Topology.resolve(
////      Node.cofToNode(tree)
////    )
//
//    // or
////    val expected =
////      seq(par(on(LiteralModel("\"other-peer\"", LiteralType.string), Nil, callL(1)), callL(1)))
//
//  }
//}
