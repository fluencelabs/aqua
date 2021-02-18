package aqua.parse

import cats.data.NonEmptyList
import cats.parse.{Parser ⇒ P, Parser0 ⇒ P0}

object Token {
  private val fSpaces = Set(' ', '\t')
  private val az = ('a' to 'z').toSet
  private val AZ = ('A' to 'Z').toSet
  private val f09 = ('0' to '9').toSet
  private val anum = az ++ AZ ++ f09
  private val f_ = Set('_')
  private val anum_ = anum ++ f_

  val ` `: P[String] = P.charsWhile(fSpaces)
  val `data`: P[Unit] = P.string("data")
  val `service`: P[Unit] = P.string("service")
  val `func`: P[Unit] = P.string("func")
  val `:`: P[Unit] = P.char(':')
  val ` : `: P[Unit] = P.char(':').surroundedBy(` `.?)
  val `name`: P[String] = (P.charIn(az) ~ P.charsWhile(anum_).?).map { case (c, s) ⇒ c.toString ++ s.getOrElse("") }
  val `Name`: P[String] = (P.charIn(AZ) ~ P.charsWhile(anum_).?).map { case (c, s) ⇒ c.toString ++ s.getOrElse("") }
  val `\n`: P[Unit] = P.char('\n')
  val `--`: P[Unit] = ` `.?.with1 *> P.string("--") <* ` `.?
  val ` \n`: P[Unit] = ((`--` *> P.charsWhile(_ != '\n')).?.void *> ` `.?.void).with1 *> `\n`
  val `,`: P[Unit] = P.char(',') <* ` `.?
  val `(`: P[Unit] = ` `.?.with1 *> P.char('(') <* ` `.?
  val `)`: P[Unit] = ` `.?.with1 *> P.char(')') <* ` `.?
  val `->`: P[Unit] = ` `.?.with1 *> P.string("->") <* ` `.?

  def comma[T](p: P[T]): P[NonEmptyList[T]] =
    P.repSep(p, `,` <* ` \n`.rep0)

  def comma0[T](p: P[T]): P0[List[T]] =
    P.repSep0(p, `,` <* ` \n`.rep0)

  def indented[T](p: P[T]): P[NonEmptyList[T]] =
    ` `.flatMap(
      indent ⇒ p.map(NonEmptyList.one) ~ (P.string(indent) *> p).rep0
    ).map {
      case (nel, l) ⇒ nel ++ l
    }
}
