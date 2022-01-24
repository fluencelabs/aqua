package aqua.compiler

import aqua.model.transform.TransformConfig
import aqua.parser.ParserError
import aqua.parser.Ast
import aqua.parser.Parser
import aqua.parser.lift.Span
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import cats.Id
import cats.data.{Chain, ValidatedNec}
import cats.instances.string.*

class AquaCompilerSpec extends AnyFlatSpec with Matchers {

  private def compileToContext = AquaCompiler.compileToContext[Id, String, String, Span.S](
    new AquaSources[Id, String, String] {
      override def sources: Id[ValidatedNec[String, Chain[(String, String)]]] = ???

      override def resolveImport(from: String, imp: String): Id[ValidatedNec[String, String]] =
        ???

      override def load(file: String): Id[ValidatedNec[String, String]] = ???
    },
    id => txt => Parser.parse(Parser.parserSchema)(txt),
    TransformConfig(wrapWithXor = false)
  )

  "aqua compiler" should "compile a simple snipped to the right context" in {


  }

}
