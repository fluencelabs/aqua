package aqua.ast

case class Gen(log: String) {}

object Gen {
  def noop = new Gen("noop")
}
