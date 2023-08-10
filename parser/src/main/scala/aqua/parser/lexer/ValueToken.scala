package aqua.parser.lexer

import aqua.parser.Expr
import aqua.parser.head.FilenameExpr
import aqua.parser.lexer.Token.*
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser.*
import aqua.types.LiteralType
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan, PToSpan, S}

import cats.parse.{Numbers, Parser as P, Parser0 as P0}
import cats.syntax.comonad.*
import cats.syntax.functor.*
import cats.{~>, Comonad, Functor}
import cats.data.{NonEmptyList, NonEmptyMap}
import cats.syntax.foldable.*
import cats.arrow.FunctionK

sealed trait ValueToken[F[_]] extends Token[F] {
  def mapK[K[_]: Comonad](fk: F ~> K): ValueToken[K]
}

case class PropertyToken[F[_]](value: ValueToken[F], properties: NonEmptyList[PropertyOp[F]])
    extends ValueToken[F] {
  override def as[T](v: T): F[T] = value.as(v)

  def mapK[K[_]: Comonad](fk: F ~> K): PropertyToken[K] =
    copy(value.mapK(fk), properties.map(_.mapK(fk)))
}

object PropertyToken {

  val property: P[ValueToken[Span.S]] =
    (ValueToken.basic ~ PropertyOp.ops.backtrack.?).map { case (v, ops) =>
      ops.fold(v)(ops => PropertyToken(v, ops))
    }

}

case class VarToken[F[_]](name: Name[F]) extends ValueToken[F] {
  override def as[T](v: T): F[T] = name.as(v)

  def mapK[K[_]: Comonad](fk: F ~> K): VarToken[K] = copy(name.mapK(fk))
}

object VarToken {
  lazy val variable: P[VarToken[Span.S]] = Name.variable.map(VarToken(_))
}

case class LiteralToken[F[_]: Comonad](valueToken: F[String], ts: LiteralType)
    extends ValueToken[F] {
  override def as[T](v: T): F[T] = valueToken.as(v)

  def mapK[K[_]: Comonad](fk: F ~> K): LiteralToken[K] = copy(fk(valueToken), ts)

  def value: String = valueToken.extract

  override def toString: String = s"$value:$ts"
}

case class CollectionToken[F[_]: Comonad](
  point: F[CollectionToken.Mode],
  values: List[ValueToken[F]]
) extends ValueToken[F] {

  override def mapK[K[_]: Comonad](fk: F ~> K): CollectionToken[K] =
    copy(fk(point), values.map(_.mapK(fk)))

  override def as[T](v: T): F[T] = point.as(v)

  def mode: CollectionToken.Mode = point.extract

  override def toString: String = s"CollectionToken(${point.extract}, $values)"
}

object CollectionToken {

  enum Mode:
    case StreamMode, OptionMode, ArrayMode

  private val left: P[Mode] =
    (`/s*`.with1 ~ (`[`.as[Mode](Mode.ArrayMode) | P.string("?[").as[Mode](Mode.OptionMode) | P
      .string("*[")
      .as[Mode](Mode.StreamMode))).map(_._2)
  private val right: P[Unit] = `]`

  val collection: P[CollectionToken[Span.S]] =
    (left.lift.between(` *`, `/s*`) ~ comma0(
      ValueToken.`value`.surroundedBy(`/s*`)
    ) <* (`/s*` *> right)).map { case (mode, vals) =>
      CollectionToken(mode, vals)
    }
}

case class CallArrowToken[F[_]: Comonad](
  name: Name[F],
  args: List[ValueToken[F]]
) extends ValueToken[F] {

  override def mapK[K[_]: Comonad](fk: F ~> K): CallArrowToken[K] =
    copy(name.mapK(fk), args.map(_.mapK(fk)))

  override def as[T](v: T): F[T] = name.as(v)
}

object CallArrowToken {

  case class CallBraces(name: Name[S], abilities: List[ValueToken[S]], args: List[ValueToken[S]])

  // {SomeAb, SecondAb} for ValueToken
  def abilities(): P[NonEmptyList[ValueToken[S]]] =
    `{` *> comma(ValueToken.`value`.surroundedBy(`/s*`)) <* `}`

  lazy val callBraces: P[CallBraces] = P
    .defer(
      Name.p ~
        abilities().? ~
        comma0(ValueToken.`value`.surroundedBy(`/s*`)).between(
          ` `.?.with1 *> `(` <* `/s*`,
          `/s*` *> `)`
        )
    )
    .map { case ((n, ab), args) =>
      CallBraces(n, ab.map(_.toList).getOrElse(Nil), args)
    }
    .withContext(
      "Missing braces '()' after the function call"
    )

  val callArrow: P[CallArrowToken[Span.S]] =
    callBraces.map { braces =>
      CallArrowToken(braces.name, braces.abilities ++ braces.args)
    }
}

case class NamedValueToken[F[_]: Comonad](
  typeName: NamedTypeToken[F],
  fields: NonEmptyMap[String, ValueToken[F]]
) extends ValueToken[F] {

  override def mapK[K[_]: Comonad](fk: F ~> K): NamedValueToken[K] =
    copy(typeName.mapK(fk), fields.map(_.mapK(fk)))

  override def as[T](v: T): F[T] = typeName.as(v)
}

object NamedValueToken {

  val dataValue: P[NamedValueToken[Span.S]] =
    (`Class`.lift ~ namedArgs)
      .withContext(
        "Missing braces '()' after the struct type"
      )
      .map { case (dn, args) =>
        NamedValueToken(NamedTypeToken(dn), args.toNem)
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

  enum BoolOp(val symbol: String):
    case Or extends BoolOp("||")
    case And extends BoolOp("&&")

  enum MathOp(val symbol: String):
    case Pow extends MathOp("**")
    case Mul extends MathOp("*")
    case Div extends MathOp("/")
    case Rem extends MathOp("%")
    case Add extends MathOp("+")
    case Sub extends MathOp("-")

  enum CmpOp(val symbol: String):
    case Gt extends CmpOp(">")
    case Gte extends CmpOp(">=")
    case Lt extends CmpOp("<")
    case Lte extends CmpOp("<=")

  enum EqOp(val symbol: String):
    case Eq extends EqOp("==")
    case Neq extends EqOp("!=")

  enum Op(val symbol: String):
    /**
     * Scala3 does not support nested enums with fields
     * so this type acrobatics is used to enable exhaustive matching check
     */
    case Math(mathOp: MathOp) extends Op(mathOp.symbol)
    case Cmp(cmpOp: CmpOp) extends Op(cmpOp.symbol)
    case Eq(eqOp: EqOp) extends Op(eqOp.symbol)
    case Bool(boolOp: BoolOp) extends Op(boolOp.symbol)

    def p: P[Unit] = P.string(symbol)

  object Op {
    val Pow = Math(MathOp.Pow)
    val Mul = Math(MathOp.Mul)
    val Div = Math(MathOp.Div)
    val Rem = Math(MathOp.Rem)
    val Add = Math(MathOp.Add)
    val Sub = Math(MathOp.Sub)

    val math = MathOp.values.map(Math(_)).toList

    val Gt = Cmp(CmpOp.Gt)
    val Gte = Cmp(CmpOp.Gte)
    val Lt = Cmp(CmpOp.Lt)
    val Lte = Cmp(CmpOp.Lte)

    val cmp = CmpOp.values.map(Cmp(_)).toList

    val Equ = Eq(EqOp.Eq)
    val Neq = Eq(EqOp.Neq)

    val eq = EqOp.values.map(Eq(_)).toList

    val And = Bool(BoolOp.And)
    val Or = Bool(BoolOp.Or)

    val bool = BoolOp.values.map(Bool(_)).toList
  }

  private def opsParser(ops: List[Op]): P[(Span, Op)] =
    P.oneOf(ops.map(op => op.p.lift.map(_.as(op))))

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

  private val pow: P[ValueToken[Span.S]] =
    infixParserRight(P.defer(ValueToken.atom), Op.Pow :: Nil)

  private val mult: P[ValueToken[Span.S]] =
    infixParserLeft(pow, Op.Mul :: Op.Div :: Op.Rem :: Nil)

  private val add: P[ValueToken[Span.S]] =
    infixParserLeft(mult, Op.Add :: Op.Sub :: Nil)

  private val compare: P[ValueToken[Span.S]] =
    infixParserNone(
      add,
      // Here order is important as
      // `Op.Gt` is prefix of `Op.Gte`
      // and `Op.Lt` is prefix of `Op.Lte`
      Op.Gte :: Op.Lte :: Op.Gt :: Op.Lt :: Nil
    )

  private val eq: P[ValueToken[Span.S]] =
    infixParserLeft(compare, Op.Equ :: Op.Neq :: Nil)

  private val and: P[ValueToken[Span.S]] =
    infixParserLeft(eq, Op.And :: Nil)

  private val or: P[ValueToken[Span.S]] =
    infixParserLeft(and, Op.Or :: Nil)

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
   * -- Logical OR is the entry point because it has the lowest priority.
   * mathExpr
   *   -> orExpr
   *
   * -- Logical OR is left associative.
   * orExpr
   *   -> andExpr OR_OP andExpr
   *
   * -- Logical AND is left associative.
   * andExpr
   *   -> eqExpr AND_OP eqExpr
   *
   * -- Equality is left associative.
   * eqExpr
   *  -> cmpExpr EQ_OP cmpExpr
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
  val value: P[ValueToken[Span.S]] = or
}

case class PrefixToken[F[_]: Comonad](
  operand: ValueToken[F],
  prefix: F[PrefixToken.Op]
) extends ValueToken[F] {

  def op: PrefixToken.Op = prefix.extract
  override def as[T](v: T): F[T] = prefix.as(v)

  override def mapK[K[_]: Comonad](fk: FunctionK[F, K]): ValueToken[K] =
    copy(operand.mapK(fk), fk(prefix))
}

object PrefixToken {

  enum Op(val symbol: String) {
    case Not extends Op("!")

    def p: P[Unit] = P.string(symbol)
  }

  private def parseOps(ops: List[Op]): P[S[Op]] =
    P.oneOf(ops.map(op => op.p.lift.map(_.as(op))))

  private def parsePrefix(basic: P[ValueToken[S]], ops: List[Op]) =
    (parseOps(ops).surroundedBy(`/s*`) ~ basic).map { case (op, vt) =>
      PrefixToken(vt, op)
    }

  val value: P[ValueToken[Span.S]] =
    parsePrefix(P.defer(ValueToken.atom), Op.Not :: Nil)
}

object ValueToken {

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
        case (None, n) ⇒ LiteralToken(fu.as(n), LiteralType.unsigned)
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

  private def brackets(basic: P[ValueToken[Span.S]]): P[ValueToken[Span.S]] =
    basic.between(`(`, `)`).backtrack

  // Basic element of value expression
  // (without property access)
  val basic = P.oneOf(
    literal.backtrack ::
      initPeerId.backtrack ::
      P.defer(CollectionToken.collection).backtrack ::
      P.defer(NamedValueToken.dataValue).backtrack ::
      P.defer(CallArrowToken.callArrow).backtrack ::
      P.defer(VarToken.variable).backtrack ::
      P.defer(PrefixToken.value).backtrack ::
      P.defer(brackets(value)).backtrack ::
      Nil
  )

  // Atomic element of math expression
  val atom: P[ValueToken[S]] = P.defer(PropertyToken.property)

  // One of entry points for parsing the whole math expression
  val `value`: P[ValueToken[Span.S]] =
    P.defer(InfixToken.value)

}
