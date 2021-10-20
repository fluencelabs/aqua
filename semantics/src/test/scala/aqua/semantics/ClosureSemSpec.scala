package aqua.semantics

import aqua.parser.expr.func.ClosureExpr
import aqua.parser.lexer.Name
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import aqua.semantics.expr.func.ClosureSem
import aqua.semantics.rules.names.NamesAlgebra
import cats.Id

class ClosureSemSpec extends AnyFlatSpec with Matchers {
  "sem" should "create right model" in {
    val expr = ClosureExpr[Id](Name("clName"))
//    implicit val namesAlg: NamesAlgebra[Id, Id] = new NamesAlgebra[Id, Id]()
//    val model = ClosureSem[Id](expr).program[Id]
  }
}
