package aqua

import aqua.parser.Ast
import aqua.parser.lift.FileSpan
import cats.data.ValidatedNec

import java.nio.file.Path

object AquaFiles {

  def apply(src: Path, imports: List[Path]): ValidatedNec[String, Map[String, Ast[FileSpan.F]]] = {

    ???
  }
}
