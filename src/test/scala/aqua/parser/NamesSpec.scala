package aqua.parser

import scala.language.implicitConversions
import aqua.parser.lexer.VarLambda
import aqua.parser.lift.Names
import cats.Id
import cats.data.NonEmptyList
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import aqua.parser.lift.LiftParser.Implicits.idLiftParser

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
  }
}
