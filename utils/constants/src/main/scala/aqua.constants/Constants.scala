package aqua.constants

import aqua.parser.expr.ConstantExpr
import aqua.raw.ConstantRaw
import aqua.raw.value.LiteralRaw

import cats.data.{NonEmptyList, Validated, ValidatedNec}
import cats.syntax.traverse.*
import cats.syntax.either.*

object Constants {

  def parse(strs: List[String]): ValidatedNec[String, List[ConstantRaw]] =
    strs.traverse(s =>
      ConstantExpr.onlyLiteral
        .parseAll(s)
        .leftMap(_ => s"Invalid constant definition '$s'.")
        .toValidatedNec
        .map { case (name, literal) =>
          ConstantRaw(name.value, LiteralRaw(literal.value, literal.ts), false)
        }
    )
}
