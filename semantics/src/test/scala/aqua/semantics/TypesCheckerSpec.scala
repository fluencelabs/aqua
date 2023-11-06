package aqua.semantics

import aqua.parser.lexer.Name
import aqua.semantics.rules.report.{ReportAlgebra, ReportInterpreter, ReportState}
import aqua.semantics.rules.types.TypesChecker
import aqua.types.{ArrayType, OptionType, ScalarType, StreamType, Type}
import cats.Id
import cats.data.State
import monocle.{Iso, Lens}
import org.scalatest.matchers.should.Matchers
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec

class TypesCheckerSpec extends AnyFlatSpec with Matchers with Inside {

  val emptyR = ReportState[Id]()

  def checkType(t: Type) = {
    type Interpreter[A] = State[ReportState[Id], A]

    given Lens[ReportState[Id], ReportState[Id]] = Iso.id[ReportState[Id]]

    given ReportAlgebra[Id, Interpreter] =
      new ReportInterpreter[Id, ReportState[Id]]

    TypesChecker.checkType[Id, ReportState[Id]](Name[Id]("t"), t).run(emptyR).value._1.errors.headOption
  }

  "checker" should "report an error on invalid types" in {
    // **string
    checkType(StreamType(StreamType(ScalarType.string))) should not be empty

    // []*string
    checkType(ArrayType(StreamType(ScalarType.string))) should not be empty

    // ?*string
    checkType(OptionType(StreamType(ScalarType.string))) should not be empty

    // ?[]*string
    checkType(OptionType(ArrayType(StreamType(ScalarType.string)))) should not be empty

    // [][]*string
    checkType(OptionType(ArrayType(StreamType(ScalarType.string)))) should not be empty

    // string
    checkType(ScalarType.string) shouldBe empty
  }
}
