package aqua.lsp

object Utils {

  def getByPosition(code: String, str: String, position: Int): Option[(Int, Int)] = {
    str.r.findAllMatchIn(code).toList.lift(position).map(r => (r.start, r.end))
  }

  extension [T](o: Option[T]) {

    def tapNone(f: => Unit): Option[T] =
      o.orElse {
        f; None
      }
  }
}
