package aqua.parser.lexer

import cats.{Comonad, Functor}
import cats.data.NonEmptyList
import cats.parse.{Accumulator0, Parser as P, Parser0 as P0}
import cats.~>

trait Token[F[_]] {
  def as[T](v: T): F[T]

  def mapK[K[_]: Comonad](fk: F ~> K): Token[K]

  def unit: F[Unit] = as(())
}

object Token {
  private val fSpaces = Set(' ', '\t')
  private val az = ('a' to 'z').toSet
  private val AZ = ('A' to 'Z').toSet
  private val f09 = ('0' to '9').toSet
  private val anum = az ++ AZ ++ f09
  private val upperAnum = AZ ++ f09
  private val f_ = Set('_')
  private val anum_ = anum ++ f_
  private val upperAnum_ = upperAnum ++ f_
  private val nl = Set('\n', '\r')

  val ` *` : P0[String] = P.charsWhile0(fSpaces)
  val ` ` : P[String] = P.charsWhile(fSpaces)
  val `const`: P[Unit] = P.string("const")
  val `data`: P[Unit] = P.string("data")
  val `import`: P[Unit] = P.string("import")
  val `module`: P[Unit] = P.string("module")
  val `declares`: P[Unit] = P.string("declares")
  val ` declares ` : P[Unit] = `declares`.surroundedBy(` `)
  val `declare`: P[Unit] = P.string("declare")
  val `_export`: P[Unit] = P.string("export")
  val `star`: P[Unit] = P.char('*')
  val `use`: P[Unit] = P.string("use")
  val `from`: P[Unit] = P.string("from")
  val ` from ` : P[Unit] = `from`.surroundedBy(` `)
  val `as`: P[Unit] = P.string("as")
  val ` as ` : P[Unit] = `as`.surroundedBy(` `)
  val `alias`: P[Unit] = P.string("alias")
  val `service`: P[Unit] = P.string("service")
  val `func`: P[Unit] = P.string("func")
  val `on`: P[Unit] = P.string("on")
  val `via`: P[Unit] = P.string("via")
  val `%init_peer_id%` : P[Unit] = P.string("%init_peer_id%")
  val `for`: P[Unit] = P.string("for")
  val `if`: P[Unit] = P.string("if")
  val `eqs`: P[Unit] = P.string("==")
  val `neq`: P[Unit] = P.string("!=")
  val `else`: P[Unit] = P.string("else")
  val `otherwise`: P[Unit] = P.string("otherwise")
  val `try`: P[Unit] = P.string("try")
  val `catch`: P[Unit] = P.string("catch")
  val `par`: P[Unit] = P.string("par")
  val `co`: P[Unit] = P.string("co")
  val `:` : P[Unit] = P.char(':')
  val ` : ` : P[Unit] = P.char(':').surroundedBy(` `.?)
  val `anum_*` : P[Unit] = P.charsWhile(anum_).void

  val NAME: P[String] = P.charsWhile(upperAnum_).string
  val `name`: P[String] = (P.charIn(az) ~ P.charsWhile(anum_).?).string

  val `Class`: P[String] = (P.charIn(AZ) ~ P.charsWhile(anum_).backtrack.?).map { case (c, s) ⇒
    c.toString ++ s.getOrElse("")
  }
  val `\n` : P[Unit] = P.string("\n\r") | P.char('\n') | P.string("\r\n")
  val `--` : P[Unit] = ` `.?.with1 *> P.string("--") <* ` `.?

  val ` \n` : P[Unit] =
    (` `.?.void *> (`--` *> P.charsWhile0(_ != '\n')).?.void).with1 *> `\n`

  val ` \n+` : P[Unit] = P.repAs[Unit, Unit](` \n`.backtrack, 1)(Accumulator0.unitAccumulator0)
  val ` \n*` : P0[Unit] = P.repAs0[Unit, Unit](` \n`.backtrack)(Accumulator0.unitAccumulator0)
  val ` : \n+` : P[Unit] = ` `.?.with1 *> `:` *> ` \n+`
  val `,` : P[Unit] = P.char(',') <* ` `.?
  val `.` : P[Unit] = P.char('.')
  val `"` : P[Unit] = P.char('"')
  val `*` : P[Unit] = P.char('*')
  val exclamation: P[Unit] = P.char('!')
  val `[]` : P[Unit] = P.string("[]")
  val `⊤` : P[Unit] = P.char('⊤')
  val `⊥` : P[Unit] = P.char('⊥')
  val `∅` : P[Unit] = P.char('∅')
  val `(` : P[Unit] = P.char('(').surroundedBy(` `.?)
  val `)` : P[Unit] = P.char(')').surroundedBy(` `.?)
  val `()` : P[Unit] = P.string("()")
  val ` -> ` : P[Unit] = P.string("->").surroundedBy(` `.?)
  val ` <- ` : P[Unit] = P.string("<-").surroundedBy(` `.?)
  val `=` : P[Unit] = P.string("=")
  val ` <<- ` : P[Unit] = P.string("<<-").surroundedBy(` `.?)
  val ` = ` : P[Unit] = P.string("=").surroundedBy(` `.?)
  val `?` : P[Unit] = P.string("?")
  val `<-` : P[Unit] = P.string("<-")
  val `/s*` : P0[Any] = ` \n+` | ` *`

  case class LiftToken[F[_]: Functor, A](point: F[A]) extends Token[F] {
    override def as[T](v: T): F[T] = Functor[F].as(point, v)
    override def mapK[K[_]: Comonad](fk: F ~> K): LiftToken[K, A] =
      copy(fk(point))
  }

  def lift[F[_]: Functor, A](point: F[A]): Token[F] = LiftToken(point)

  def comma[T](p: P[T]): P[NonEmptyList[T]] =
    P.repSep(p, `,` <* ` \n+`.rep0)

  def comma0[T](p: P[T]): P0[List[T]] =
    P.repSep0(p, `,` <* ` \n+`.rep0)

  def asOpt[T](p: P[T]): P[(T, Option[T])] =
    p ~ (` as `.backtrack *> p).?
}
