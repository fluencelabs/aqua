package aqua.parser

trait Expression[F[_], L] {
  def context: L
}
