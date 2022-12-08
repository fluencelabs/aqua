package aqua.constants

import scala.util.Left
import aqua.parser.expr.ConstantExpr
import aqua.raw.ConstantRaw
import aqua.raw.value.LiteralRaw
import cats.data.NonEmptyList
import cats.data.Validated

object Constants {
  def parse(strs: List[String]) = {
    val parsed = strs.map(s => ConstantExpr.onlyLiteral.parseAll(s))

    val errors = parsed.zip(strs).collect { case (Left(er), str) =>
      str
    }

    NonEmptyList
      .fromList(errors)
      .fold(
        Validated.validNel[String, List[ConstantRaw]](parsed.collect { case Right(v) =>
          ConstantRaw(v._1.value, LiteralRaw(v._2.value, v._2.ts), false)
        })
      ) { errors =>
        val errorMsgs = errors.map(str => s"Invalid constant definition '$str'.")
        Validated.invalid(errorMsgs)
      }
  }
}
