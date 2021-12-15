package aqua.io

// Uses to print outputs in CLI
// TODO: add F[_], cause it is effect
object OutputPrinter {

  def print(str: String): Unit = {
    println(str)
  }

  def error(str: String): Unit = {
    println(str)
  }
}
