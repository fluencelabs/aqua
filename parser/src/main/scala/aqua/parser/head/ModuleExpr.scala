package aqua.parser.head

import aqua.parser.lexer.Token.*
import aqua.parser.lexer.{CustomTypeToken, Literal, Name, Value}
import aqua.parser.lift.LiftParser
import cats.Comonad
import cats.parse.Parser

case class ModuleExpr[F[_]](
  name: CustomTypeToken[F],
  exportNames: List[Name[F]],
  exportCustom: List[CustomTypeToken[F]]
) extends HeaderExpr[F]

object ModuleExpr extends HeaderExpr.Leaf {

  override def p[F[_]: LiftParser: Comonad]: Parser[ModuleExpr[F]] =
    (`module` *> ` ` *> CustomTypeToken.ct[F] ~
      (` ` *> `exports` *> comma[Either[Name[F], CustomTypeToken[F]]](
        Name.p[F].map(Left(_)) | CustomTypeToken.ct[F].map(Right(_))
      )).map(_.toList).?).map {
      case (name, None) =>
        ModuleExpr(name, Nil, Nil)
      case (name, Some(exportMembers)) =>
        ModuleExpr(
          name,
          exportMembers.collect { case Left(x) => x },
          exportMembers.collect { case Right(x) => x }
        )
    }

}
