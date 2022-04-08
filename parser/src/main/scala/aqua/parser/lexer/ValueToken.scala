package aqua.parser.lexer

import aqua.parser.Expr
import aqua.parser.head.FilenameExpr
import aqua.parser.lexer.Token.*
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser.*
import aqua.types.LiteralType
import cats.parse.{Numbers, Parser as P, Parser0 as P0}
import cats.syntax.comonad.*
import cats.syntax.functor.*
import cats.{~>, Comonad, Functor}
import cats.data.NonEmptyList
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan, PToSpan, S}

sealed trait ValueToken[F[_]] extends Token[F] {
  def mapK[K[_]: Comonad](fk: F ~> K): ValueToken[K]
}

case class VarToken[F[_]](name: Name[F], lambda: List[LambdaOp[F]] = Nil) extends ValueToken[F] {
  override def as[T](v: T): F[T] = name.as(v)

  def mapK[K[_]: Comonad](fk: F ~> K): VarToken[K] = copy(name.mapK(fk), lambda.map(_.mapK(fk)))
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
}

object CollectionToken {

  enum Mode:
    case StreamMode, OptionMode, ArrayMode

  val collection: P[CollectionToken[Span.S]] =
    ((
      `*[`.as[Mode](Mode.StreamMode) |
        `?[`.as[Mode](Mode.OptionMode) |
        `[`.as[Mode](Mode.ArrayMode)
    ).lift ~ (P
      .defer(ValueToken.`_value`)
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
        ~ comma0(ValueToken.`_value`.surroundedBy(`/s*`)).between(`(` <* `/s*`, `/s*` *> `)`))
        .withContext(
          "Missing braces '()' after the function call"
        )).map { case (ab, (fn, args)) =>
      CallArrowToken(ab, fn, args)
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

  enum Op(val weight: Int):
    case Pow extends Op(10)
    case Mul extends Op(6)
    case Div extends Op(6)
    case Rem extends Op(6)
    case Add extends Op(3)
    case Sub extends Op(3)
    case Gt extends Op(1)
    case Gte extends Op(1)
    case Lt extends Op(1)
    case Lte extends Op(1)

  val ops: List[P[Span.S[Op]]] =
    List(
      `**`.as(Op.Pow),
      `*`.as(Op.Mul),
      `/`.as(Op.Div),
      `%`.as(Op.Rem),
      `-`.as(Op.Sub),
      `+`.as(Op.Add),
      `>`.as(Op.Gt),
      `>=`.as(Op.Gte),
      `<`.as(Op.Lt),
      `<=`.as(Op.Lte)
    ).map(_.lift)

  private def infixParserLeft(basic: P[ValueToken[S]], ops: List[P[Op]]) =
    (basic ~ (P
      .oneOf(ops.map(_.lift))
      .surroundedBy(`/s*`) ~ basic).backtrack.rep0).map { case (vt, list) =>
      list.foldLeft(vt) { case (acc, (op, value)) =>
        InfixToken(acc, value, op)
      }
    }

  private def infixParserRight(basic: P[ValueToken[S]], ops: List[P[Op]]): P[ValueToken[S]] = P.recursive { recurse =>
    (basic ~ (P.oneOf(ops.map(_.lift)).surroundedBy(`/s*`) ~ recurse).backtrack.?).map {
      case (vt, Some((op, recVt))) =>
        InfixToken(vt, recVt, op)
      case (vt, None) =>
        vt
    }
  }

  private def infixParserNone(basic: P[ValueToken[S]], ops: List[P[Op]]) =
    (basic ~ P.oneOf(ops.map(_.lift)).surroundedBy(`/s*`) ~ basic).map {
      case ((leftVt, op), rightVt) => InfixToken(leftVt, rightVt, op)
    }

  val pow: P[ValueToken[Span.S]] =
    infixParserRight(ValueToken.atom, `**`.as(Op.Pow) :: Nil)

  val mult: P[ValueToken[Span.S]] =
    infixParserLeft(pow, `*`.as(Op.Mul) :: `/`.as(Op.Div) :: `%`.as(Op.Div) :: Nil)

  val add: P[ValueToken[Span.S]] =
    infixParserLeft(mult, `+`.as(Op.Add) :: `-`.as(Op.Sub) :: Nil)

  val compare: P[ValueToken[Span.S]] =
    infixParserNone(add, `>`.as(Op.Gt) :: `>=`.as(Op.Gte) :: `<`.as(Op.Lt) :: `<=`.as(Op.Lte) :: Nil)

  val arithmeticExpr = add

  val logicExpr: P[ValueToken[Span.S]] = P.recursive { recurse => compare | ValueToken.brackets(recurse) | ValueToken.bool }

  val mathExpr =  logicExpr.backtrack | arithmeticExpr
}

object ValueToken {

  val varLambda: P[VarToken[Span.S]] =
    (Name.dotted ~ LambdaOp.ops.?).map { case (n, l) ⇒
      VarToken(n, l.fold[List[LambdaOp[Span.S]]](Nil)(_.toList))
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
    P.oneOf(float.backtrack :: num.backtrack :: string :: Nil)

  def brackets(basic: P[ValueToken[Span.S]]): P[ValueToken[Span.S]] = basic.between(`(`, `)`).backtrack

  val atom: P[ValueToken[S]] = P.oneOf(
    literal.backtrack ::
      initPeerId.backtrack ::
      P.defer(
        CollectionToken.collection
      ) ::
      P.defer(CallArrowToken.callArrow).backtrack ::
      P.defer(brackets(InfixToken.arithmeticExpr)) ::
      varLambda ::
      Nil
  )

  def `_value`: P[ValueToken[Span.S]] = {
    InfixToken.mathExpr
  }

  val `value`: P[ValueToken[Span.S]] =
    P.defer(`_value`)

}
