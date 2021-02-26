package aqua

import aqua.parser.{DefFunc, FuncOp}
import aqua.parser.lexer.VarLambda
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

  private implicit def setToMapS(set: Set[String]): Map[String, Id[String]] = set.map(s => s -> s).toMap
  private implicit def setToMapV(set: Set[String]): Map[String, Id[VarLambda]] = set.map(s => s -> VarLambda(s)).toMap

  "names" should "extract from funcops" in {
    namesP(" func()") should be(Names[Id](expectArrows = Set("func")))
    namesP(" fn(32)") should be(Names[Id](expectArrows = Set("fn")))
    namesP(" fn(s)") should be(Names[Id](expectArrows = Set("fn"), importData = Set("s")))
    namesP(" x <- fn(s)") should be(Names[Id](expectArrows = Set("fn"), importData = Set("s"), exportData = Set("x")))
    namesP(" x <- fn(s)\n y <- fn(z)") should be(
      Names[Id](expectArrows = Set("fn"), importData = Set("s", "z"), exportData = Set("x", "y"))
    )
    namesP(" x <- fn(s)\n y <- fn(x)") should be(
      Names[Id](expectArrows = Set("fn"), importData = Set("s"), exportData = Set("x", "y"))
    )
    namesP(""" Peer 42
             | x <- Peer.id()
             | y <- Op.identity()""".stripMargin) should be(
      Names[Id](exportData = Set("x", "y"), resolvedAbilities = Set("Peer"), expectedAbilities = Set("Op"))
    )
    namesP(""" on p:
             |   x <- Peer.id()
             |   Op "op"
             | arr()""".stripMargin) should be(
      Names[Id](
        importData = Set("p"),
        exportData = Set("x"),
        expectedAbilities = Set("Peer"),
        expectArrows = Set("arr")
      )
    )
    namesP(""" on p:
             |   x <- Peer.id(23, p, k)
             |   Op z
             | arr()""".stripMargin) should be(
      Names[Id](
        importData = Set("p", "k", "z"),
        exportData = Set("x"),
        expectedAbilities = Set("Peer"),
        expectArrows = Set("arr")
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
        expectArrows = Set("finish"),
        expectedAbilities = Set("Other"),
        importData = Set("v")
      )
    )
  }
}
