package aqua.parser.lexer

import aqua.parser.Expr
import aqua.parser.head.FilenameExpr
import aqua.parser.lexer.Token.*
import aqua.parser.lexer.ValueToken.{initPeerId, literal}
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser.*
import aqua.types.LiteralType
import cats.parse.{Numbers, Parser as P, Parser0 as P0}
import cats.syntax.comonad.*
import cats.syntax.functor.*
import cats.{Comonad, Functor, ~>}
import cats.data.{NonEmptyList, NonEmptyMap}
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan, PToSpan, S}

sealed trait ValueToken[F[_]] extends Token[F] {
  def mapK[K[_]: Comonad](fk: F ~> K): ValueToken[K]
}

case class VarToken[F[_]](name: Name[F], property: List[PropertyOp[F]] = Nil)
    extends ValueToken[F] {
  override def as[T](v: T): F[T] = name.as(v)

  def mapK[K[_]: Comonad](fk: F ~> K): VarToken[K] = copy(name.mapK(fk), property.map(_.mapK(fk)))
}

case class LiteralToken[F[_]: Comonad](valueToken: F[String], ts: LiteralType)
    extends ValueToken[F] {
  override def as[T](v: T): F[T] = valueToken.as(v)

  def mapK[K[_]: Comonad](fk: F ~> K): LiteralToken[K] = copy(fk(valueToken), ts)

  def value: String = valueToken.extract

  override def toString: String = s"$value"
}

case class CollectionToken[F[_]: Comonad](
  point: F[CollectionToken.Mode],
  values: List[ValueToken[F]]
) extends ValueToken[F] {

  override def mapK[K[_]: Comonad](fk: F ~> K): ValueToken[K] =
    copy(fk(point), values.map(_.mapK(fk)))

  override def as[T](v: T): F[T] = point.as(v)

  def mode: CollectionToken.Mode = point.extract

  override def toString: String = s"CollectionToken(${point.extract}, $values)"
}

object CollectionToken {

  enum Mode:
    case StreamMode, OptionMode, ArrayMode

  val collection: P[CollectionToken[Span.S]] =
    ((
      `*[`.as[Mode](Mode.StreamMode) |
        `?[`.as[Mode](Mode.OptionMode) |
        `[`.as[Mode](Mode.ArrayMode)
    ).lift ~ (ValueToken.`value`
      .repSep0(`,`) <* `]`)).map { case (mode, vals) =>
      CollectionToken(mode, vals)
    }
}

case class CallArrowToken[F[_]: Comonad](
  ability: Option[Ability[F]],
  funcName: Name[F],
  args: List[ValueToken[F]]
) extends ValueToken[F] {

  override def mapK[K[_]: Comonad](fk: F ~> K): CallArrowToken[K] =
    copy(ability.map(_.mapK(fk)), funcName.mapK(fk), args.map(_.mapK(fk)))

  override def as[T](v: T): F[T] = funcName.as(v)
}

object CallArrowToken {

  val callArrow: P[CallArrowToken[Span.S]] =
    ((Ability.dotted <* `.`).?.with1 ~
      (Name.p
        ~ comma0(ValueToken.`value`.surroundedBy(`/s*`))
          .between(` `.?.with1 *> `(` <* `/s*`, `/s*` *> `)`))
        .withContext(
          "Missing braces '()' after the function call"
        )).map { case (ab, (fn, args)) =>
      CallArrowToken(ab, fn, args)
    }
}

case class DataValueToken[F[_]: Comonad](
  typeName: CustomTypeToken[F],
  fields: NonEmptyMap[String, ValueToken[F]]
) extends ValueToken[F] {

  override def mapK[K[_]: Comonad](fk: F ~> K): DataValueToken[K] =
    copy(typeName.mapK(fk), fields.map(_.mapK(fk)))

  override def as[T](v: T): F[T] = typeName.as(v)
}

object DataValueToken {

  val dataValue: P[DataValueToken[Span.S]] =
      (`Class`.lift
        ~ comma(((`name` <* (` `.?.with1 *> `=` *> ` `.?)).with1 ~ ValueToken.`value`).surroundedBy(`/s*`))
          .between(` `.?.with1 *> `(` <* `/s*`, `/s*` *> `)`))
        .withContext(
          "Missing braces '()' after the struct type"
        ).map { case (dn, args) =>
      DataValueToken(CustomTypeToken(dn), NonEmptyMap.of(args.head, args.tail:_*))
    }
}

// Two values as operands, with an infix between them
case class InfixToken[F[_]: Comonad](
  left: ValueToken[F],
  right: ValueToken[F],
  infix: F[InfixToken.Op]
) extends ValueToken[F] {

  val op: InfixToken.Op = infix.extract

  override def mapK[K[_]: Comonad](fk: F ~> K): ValueToken[K] =
    copy(left.mapK(fk), right.mapK(fk), fk(infix))

  override def as[T](v: T): F[T] = infix.as(v)

  override def toString: String = s"($op $left $right)"
}

object InfixToken {

  import ValueToken._

  enum Op(val symbol: String):
    case Pow extends Op("**")
    case Mul extends Op("*")
    case Div extends Op("/")
    case Rem extends Op("%")
    case Add extends Op("+")
    case Sub extends Op("-")
    case Gt extends Op(">")
    case Gte extends Op(">=")
    case Lt extends Op("<")
    case Lte extends Op("<=")

  private def opsParser(ops: List[Op]): P[(Span, Op)] =
    P.oneOf(ops.map(op => P.string(op.symbol).lift.map(s => s.as(op))))

  // Parse left-associative operations `basic (OP basic)*`.
  // We use this form to avoid left recursion.
  private def infixParserLeft(basic: P[ValueToken[S]], ops: List[Op]) =
    (basic ~ (opsParser(ops).surroundedBy(`/s*`) ~ basic).backtrack.rep0).map { case (vt, list) =>
      list.foldLeft(vt) { case (acc, (op, value)) =>
        InfixToken(acc, value, op)
      }
    }

  // Parse right-associative operations: `basic OP recursive`.
  private def infixParserRight(basic: P[ValueToken[S]], ops: List[Op]): P[ValueToken[S]] =
    P.recursive { recurse =>
      (basic ~ (opsParser(ops).surroundedBy(`/s*`) ~ recurse).backtrack.?).map {
        case (vt, Some((op, recVt))) =>
          InfixToken(vt, recVt, op)
        case (vt, None) =>
          vt
      }
    }

  // Parse non-associative operations: `basic OP basic`.
  private def infixParserNone(basic: P[ValueToken[S]], ops: List[Op]) =
    (basic ~ (opsParser(ops).surroundedBy(`/s*`) ~ basic).backtrack.?) map {
      case (leftVt, Some((op, rightVt))) =>
        InfixToken(leftVt, rightVt, op)
      case (vt, None) =>
        vt
    }

  def brackets(basic: P[ValueToken[Span.S]]): P[ValueToken[Span.S]] =
    basic.between(`(`, `)`).backtrack

  // One element of math expression
  private val atom: P[ValueToken[S]] = P.oneOf(
    literal.backtrack ::
      initPeerId.backtrack ::
      P.defer(
        CollectionToken.collection
      ) ::
      P.defer(DataValueToken.dataValue).backtrack ::
      P.defer(CallArrowToken.callArrow).backtrack ::
      P.defer(brackets(InfixToken.mathExpr)) ::
      varProperty ::
      Nil
  )

  private val pow: P[ValueToken[Span.S]] =
    infixParserRight(atom, Op.Pow :: Nil)

  private val mult: P[ValueToken[Span.S]] =
    infixParserLeft(pow, Op.Mul :: Op.Div :: Op.Rem :: Nil)

  private val add: P[ValueToken[Span.S]] =
    infixParserLeft(mult, Op.Add :: Op.Sub :: Nil)

  private val compare: P[ValueToken[Span.S]] =
    infixParserNone(
      add,
      Op.Gt :: Op.Gte :: Op.Lt :: Op.Lte :: Nil
    )

  /**
   * The math expression parser.
   *
   * Fist, the general idea. We'll consider only the expressions with operations `+`, `-`, and `*`, with bracket
   * support. This syntax would typically be defined as follows:
   *
   * mathExpr ->
   *   number
   *   | mathExpr + mathExpr
   *   | mathExpr - mathExpr
   *   | mathExpr * mathExpr
   *   | ( mathExpr )
   *
   * However, this grammar is ambiguous. For example, there are valid two parse trees for string `1 + 3 * 4`:
   *
   * 1)
   *      +
   *    /   \
   *   1    *
   *      /   \
   *     3     4
   * 2)
   *       *
   *     /   \
   *    +    4
   *  /   \
   * 1     3
   *
   * We will instead define the grammar in a way that only allows a single possible parse tree.
   * This parse tree will have the correct precedence and associativity of the operations built in.
   *
   * The intuition behind such grammar rules is as follows.
   * For example, 1 + 2 - 3 * 4 + 5 * (6 + 1) can be thought of as a string of the form
   *             `_ + _ - _____ + ___________`, where
   * the underscores denote products of one or more numbers or bracketed expressions.
   *
   * In other words, an expression of this form is *the sum of several products*. We can encode this, as
   * well as the fact that addition and subtraction are left-associative, as this rule:
   * addExpr
   *   -> addExpr ADD_OP multExpr
   *   | multExpr
   *
   * If we parse the string like this, then precedence of `+`, `-`, and `*` will work correctly out of the box,
   *
   * The grammar below expresses the operator precedence and associativity we expect from math expressions:
   *
   * -- Comparison is the entry point because it has the lowest priority.
   * mathExpr
   *   -> cmpExpr
   *
   * -- Comparison isn't an associative operation so it's not a recursive definition.
   * cmpExpr
   *   -> addExpr CMP_OP addExpr
   *   | addExpr
   *
   * -- Addition is a left associative operation, so it calls itself recursively on the left.
   * -- To avoid the left recursion problem this is implemented as `multExpr (ADD_OP multExpr)*`.
   * addExpr
   *   -> addExpr ADD_OP multExpr
   *   | multExpr
   *
   * -- Multiplication is also a left associative operation, so it calls itself recursively on the left.
   * -- To avoid the left recursion problem actually we employ the `expExpr (ADD_OP expExpr)*` form.
   * multExpr
   *   -> multExpr MULT_OP expExpr
   *   |  expExpr
   *
   * -- Exponentiation is a right associative operation, so it calls itself recursively on the right.
   * expExpr
   *   -> atom EXP_OP exprExpr
   *   | atom
   *
   * -- Atomic expression is an expression that can be parsed independently of what's going on around it.
   * -- For example, an expression in brackets will be parsed the same way regardless of what part of the
   * -- expression it's in.
   * atom
   *   -> number
   *   | literal
   *   | ...
   *   | ( mathExpr )
   */
  val mathExpr: P[ValueToken[Span.S]] = compare
}

object ValueToken {

  val varProperty: P[VarToken[Span.S]] =
    (Name.dotted ~ PropertyOp.ops.?).map { case (n, l) ⇒
      VarToken(n, l.fold[List[PropertyOp[Span.S]]](Nil)(_.toList))
    }

  val bool: P[LiteralToken[Span.S]] =
    P.oneOf(
      ("true" :: "false" :: Nil)
        .map(t ⇒ P.string(t).lift.map(fu => LiteralToken(fu.as(t), LiteralType.bool)))
    ) <* P.not(`anum_*`)

  val initPeerId: P[LiteralToken[Span.S]] =
    `%init_peer_id%`.string.lift.map(LiteralToken(_, LiteralType.string))

  val minus = P.char('-')
  val dot = P.char('.')

  val num: P[LiteralToken[Span.S]] =
    (minus.?.with1 ~ Numbers.nonNegativeIntString).lift.map(fu =>
      fu.extract match {
        case (Some(_), n) ⇒ LiteralToken(fu.as(s"-$n"), LiteralType.signed)
        case (None, n) ⇒ LiteralToken(fu.as(n), LiteralType.number)
      }
    )

  val float: P[LiteralToken[Span.S]] =
    (minus.?.with1 ~ (Numbers.nonNegativeIntString <* dot) ~ Numbers.nonNegativeIntString).string.lift
      .map(LiteralToken(_, LiteralType.float))

  val charsWhileQuotes = P.charsWhile0(_ != '"')

  // TODO make more sophisticated escaping/unescaping
  val string: P[LiteralToken[Span.S]] =
    (`"` *> charsWhileQuotes <* `"`).string.lift
      .map(LiteralToken(_, LiteralType.string))

  val literal: P[LiteralToken[Span.S]] =
    P.oneOf(bool.backtrack :: float.backtrack :: num.backtrack :: string :: Nil)

  // One of entry points for parsing the whole math expression
  val `value`: P[ValueToken[Span.S]] =
    P.defer(InfixToken.mathExpr)

}
