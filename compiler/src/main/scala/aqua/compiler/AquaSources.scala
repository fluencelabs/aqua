package aqua.compiler

import cats.data.{Chain, ValidatedNec}

trait AquaSources[F[_], Err, I] {
  // Read the sources in the sources directory as I, String pairs
  def sources: F[ValidatedNec[Err, Chain[(I, String)]]]

  // Resolve id of the imported imp string from I file
  def resolve(from: I, imp: String): F[ValidatedNec[Err, I]]

  // Load file by its resolved I
  def load(file: I): F[ValidatedNec[Err, String]]
}
