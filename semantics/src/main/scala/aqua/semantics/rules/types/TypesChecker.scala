package aqua.semantics.rules.types

import aqua.parser.lexer.Token
import aqua.semantics.rules.report.ReportAlgebra
import aqua.types.{BoxType, StreamType, Type}
import cats.data.State

object TypesChecker {

  // check if this type can exist in aqua code
  def checkType[S[_], X](name: Token[S], `type`: Type)(using report: ReportAlgebra[S, State[X, *]]): State[X, Unit] = {
    `type` match {
      case StreamType(StreamType(_)) =>
        report.error(name, "Cannot define stream of streams")
      case b: BoxType =>
        b.element match {
          case _: StreamType =>
            report.error(name, "Cannot define type. Stream cannot be in a boxed type")
          case bt: BoxType =>
            checkType(name, bt)
          case _ =>
            State.pure(())
        }
      case t =>
        State.pure(())
    }
  }
}
