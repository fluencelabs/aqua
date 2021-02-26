package aqua

import aqua.parser.{BasicType, DefFunc, FuncOp}
import aqua.parser.lexer.{Literal, Value, VarLambda}
import aqua.parser.lift.LiftParser.Implicits.idLiftParser
import cats.Id
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

class NamesSpec extends AnyFlatSpec with Matchers with EitherValues {
  private val namesId = Names.funcOps[Id](_)
  private val funcOpsP = (v: String) => FuncOp.body[Id].parseAll(v).right.value
  private val namesP = (v: String) => namesId(funcOpsP(v))

  private implicit def setToAccS(set: Set[String]): Names.Acc[Id, String] =
    set.map(Names.Acc.str[Id]).foldLeft(Names.Acc.empty[Id, String])(_ add _)

  private def setToAccVar(set: Set[String]): Names.Acc[Id, VarLambda] =
    set.map(s => Names.Acc.one[Id, VarLambda](s, VarLambda(s))).foldLeft(Names.Acc.empty[Id, VarLambda])(_ add _)

  private implicit def setToAccVal(set: Set[String]): Names.Acc[Id, Value] =
    set
      .map(s => Names.Acc.one[Id, Value](s, VarLambda(s)))
      .foldLeft(Names.Acc.empty[Id, Value])(_ add _)

  "names" should "extract from funcops" in {
    namesP(" func()") should be(Names[Id](expectedArrows = Set("func")))
    namesP(" fn(32)") should be(Names[Id](expectedArrows = Set("fn")))
    namesP(" fn(s)") should be(Names[Id](expectedArrows = Set("fn"), importData = Set("s")))
    namesP(" x <- fn(s)") should be(Names[Id](expectedArrows = Set("fn"), importData = Set("s"), exportData = Set("x")))
    namesP(" x <- fn(s)\n y <- fn2(z)") should be(
      Names[Id](expectedArrows = Set("fn", "fn2"), importData = Set("s", "z"), exportData = Set("x", "y"))
    )
    namesP(" x <- fn(s)\n y <- gn(x)") should be(
      Names[Id](expectedArrows = Set("fn", "gn"), importData = Set("s"), exportData = Set("x", "y"))
    )
    namesP(""" Peer 42
             | x <- Cop.id()
             | y <- Op.identity()""".stripMargin) should be(
      Names[Id](exportData = Set("x", "y"), resolvedAbilities = Set("Peer"), unresolvedAbilities = Set("Op", "Cop"))
    )
    namesP(""" on p:
             |   x <- Peer.id()
             |   Op "op"
             | arr()""".stripMargin) should be(
      Names[Id](
        importData = Set("p"),
        exportData = Set("x"),
        unresolvedAbilities = Set("Peer"),
        expectedArrows = Set("arr")
      )
    )
    namesP(""" on p:
             |   x <- Peer.id(23, p, k)
             |   Op z
             | arr()""".stripMargin) should be(
      Names[Id](
        importData = Set("p", "k", "z"),
        exportData = Set("x"),
        unresolvedAbilities = Set("Peer"),
        expectedArrows = Set("arr")
      )
    )
  }

  "names" should "see noo free names in correct function" in {
    val func =
      """func getTime(peer: PeerId, ret: i32 -> ()) -> string:
        | on peer:
        |   Peer "peer"
        |   t <- Peer.timestamp()
        | ret(t)""".stripMargin

    val funcAst = DefFunc.`deffunc`[Id].parseAll(func).right.value

    Names.funcNames(funcAst) should be(
      Names[Id]()
    )
  }

  "names" should "get free abilities, vars in functions" in {
    val func =
      """func getTime(peer: PeerId, ret: i32 -> ()) -> string:
        | on peer:
        |   Peer "peer"
        |   t <- Peer.timestamp()
        |   Other.call()
        |   finish()
        | ret(t, v)""".stripMargin

    val funcAst = DefFunc.`deffunc`[Id].parseAll(func).right.value

    Names.funcNames(funcAst) should be(
      Names[Id](
        expectedArrows = Set("finish"),
        unresolvedAbilities = Set("Other"),
        importData = Set("v")
      )
    )
  }
}
