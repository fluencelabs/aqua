package aqua.helpers.ext

object Extension {

  val aqua = ".aqua"

  // def add '.aqua' extension if there is no one
  def add(path: String): String = {
    if (path.endsWith(aqua))
      path
    else
      path + aqua
  }
}
