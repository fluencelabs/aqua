package aqua

import aqua.files.FileModuleId
import aqua.parser.lift.{FileSpan, Span}
import aqua.parser.{Ast, Parser, ParserError}
import cats.data.*
import cats.parse.LocationMap
import cats.{Comonad, Eval, Monad, Monoid, Order, ~>}

object SpanParser extends scribe.Logging {
  def parser: FileModuleId => String => ValidatedNec[ParserError[FileSpan.F], Ast[FileSpan.F]] = {
    id => {
      source => {
        logger.trace(s"creating parser for $id...")
        val nat = new (Span.F ~> FileSpan.F) {
          override def apply[A](span: Span.F[A]): FileSpan.F[A] = {
            (
              FileSpan(id.file.absolute.toString, Eval.later(LocationMap(source)), span._1),
              span._2
            )
          }
        }
        import Span.spanLiftParser
        val parser = Parser.natParser(Parser.spanParser, nat)(source)
        logger.trace("parser created")
        parser
      }
    }
  }
}
