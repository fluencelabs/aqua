package aqua.io

// Uses to print outputs in CLI
object OutputPrinter {

  def print(str: String): Unit = {
    println(str)
  }

  def error(str: String): Unit = {
    println(str)
  }
}
