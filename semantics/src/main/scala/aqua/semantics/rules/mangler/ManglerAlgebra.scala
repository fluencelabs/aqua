package aqua.semantics.rules.mangler

trait ManglerAlgebra[Alg[_]] {
  def rename(name: String): Alg[String]
}
