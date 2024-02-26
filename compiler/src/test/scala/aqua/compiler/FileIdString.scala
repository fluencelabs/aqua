package aqua.compiler

import aqua.semantics.FileId

object FileIdString {
  given FileId[String] with {
    override def show(t: String): String = t

    override def compare(x: String, y: String): Int = x.compare(y)
  }
}
