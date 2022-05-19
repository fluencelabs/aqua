package aqua.io

import cats.effect.std.Console

// Uses to print outputs in CLI
// TODO: add F[_], cause it is effect
object OutputPrinter {

  def print(str: String): Unit = {
    println(str)
  }

  def errorF[F[_]: Console](str: String): F[Unit] = {
    Console[F].errorln(scala.Console.RED + str + scala.Console.RESET)
  }
}
