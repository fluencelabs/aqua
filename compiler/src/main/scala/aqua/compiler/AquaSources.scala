package aqua.compiler

import cats.data.{Chain, ValidatedNec}

trait AquaSources[F[_], E, I] {
  def sources: F[ValidatedNec[E, Chain[(I, String)]]]

  def resolve(from: I, imp: String): F[ValidatedNec[E, I]]

  def load(file: I): F[ValidatedNec[E, String]]
}
