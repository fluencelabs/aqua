/*
 * Copyright (C) 2024  Fluence DAO
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package aqua

import aqua.AquaSpec.spanToId
import aqua.parser.expr.*
import aqua.parser.expr.func.*
import aqua.parser.head.{FromExpr, UseFromExpr}
import aqua.parser.lexer.*
import aqua.parser.lexer.InfixToken.Op.*
import aqua.parser.lexer.InfixToken.Op as InfixOp
import aqua.parser.lexer.PrefixToken.Op.*
import aqua.parser.lexer.PrefixToken.Op as PrefixOp
import aqua.parser.lexer.Token.LiftToken
import aqua.parser.lift.LiftParser.given
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{given, *}
import aqua.types.LiteralType.{bool, number, signed, string, unsigned}
import aqua.types.{LiteralType, ScalarType}

import cats.data.NonEmptyList
import cats.syntax.bifunctor.*
import cats.{Id, ~>}
import cats.~>
import org.scalatest.EitherValues
import scala.collection.mutable
import scala.language.implicitConversions

object AquaSpec {

  def spanToId: Span.S ~> Id = new (Span.S ~> Id) {

    override def apply[A](span: Span.S[A]): Id[A] = {
      span._2
    }
  }

  def toName(str: String): Name[Id] = Name[Id](str)

  def toNameOp(str: Option[String]): Option[Name[Id]] = str.map(s => toName(s))

  def toAb(str: String): Ability[Id] = Ability[Id](str)

  def toQName(str: String): QName[Id] = QName[Id](
    str,
    NonEmptyList.fromListUnsafe(str.split("\\.").toList)
  )

  def toQNameAs(name: String, rename: Option[String]): QName.As[Id] =
    QName.As[Id](toQName(name), rename.map(toQName))

  def toVar(name: String): VarToken[Id] = VarToken[Id](toName(name))

  def toVarOp(name: Option[String]): Option[VarToken[Id]] =
    name.map(toVar)

  def toVarLambda(name: String, fields: List[String]): ValueToken[Id] =
    NonEmptyList
      .fromList(fields)
      .fold(toVar(name))(fs =>
        PropertyToken(
          toVar(name),
          fs.map(IntoField[Id].apply)
        )
      )

  def toVarIndex(name: String, idx: Int): PropertyToken[Id] =
    PropertyToken[Id](
      VarToken[Id](toName(name)),
      NonEmptyList.one(IntoIndex[Id](toNumber(idx).unit, Some(toNumber(idx))))
    )

  def toLiteral(name: String, t: LiteralType): LiteralToken[Id] = LiteralToken[Id](name, t)

  def toNumber(n: Int): LiteralToken[Id] = LiteralToken[Id](n.toString, LiteralType.forInt(n))
  def toBool(n: Boolean): LiteralToken[Id] = LiteralToken[Id](n.toString, bool)
  def toStr(n: String): LiteralToken[Id] = LiteralToken[Id]("\"" + n + "\"", string)

  def toArr(arr: List[ValueToken[Id]]): CollectionToken[Id] =
    CollectionToken[Id](CollectionToken.Mode.ArrayMode, arr)

  def toNamedType(str: String): NamedTypeToken[Id] = NamedTypeToken[Id](str)
  def toArrayType(str: String): ArrayTypeToken[Id] = ArrayTypeToken[Id]((), str)

  def toArrowType(
    args: List[BasicTypeToken[Id]],
    res: Option[BasicTypeToken[Id]],
    abilities: List[NamedTypeToken[Id]] = Nil
  ): ArrowTypeToken[Id] =
    ArrowTypeToken[Id]((), args.map(None -> _), res.toList, abilities)

  def toNamedArrow(
    args: List[(String, TypeToken[Id])],
    res: List[BasicTypeToken[Id]],
    abilities: List[NamedTypeToken[Id]] = Nil
  ): ArrowTypeToken[Id] =
    ArrowTypeToken[Id]((), args.map(ab => Some(Name[Id](ab._1)) -> ab._2), res, abilities)

  def toNamedArg(str: String, customType: String): Arg[Id] =
    Arg[Id](str, toNamedType(customType))

  def toArg(str: String, typeToken: TypeToken[Id]): Arg[Id] = Arg[Id](str, typeToken)

  def toArgSc(str: String, scalarType: ScalarType): Arg[Id] =
    Arg[Id](str, scToBt(scalarType))

  def scToBt(sc: ScalarType): ScalarTypeToken[Id] = ScalarTypeToken[Id](sc)

  val boolSc: ScalarTypeToken[Id] = ScalarTypeToken[Id](ScalarType.bool)
  val stringSc: ScalarTypeToken[Id] = ScalarTypeToken[Id](ScalarType.string)

  given Conversion[String, Name[Id]] = toName
  given Conversion[String, NamedTypeToken[Id]] = toNamedType
  given Conversion[Int, LiteralToken[Id]] = toNumber
  given Conversion[ScalarType, ScalarTypeToken[Id]] = scToBt
}

trait AquaSpec extends EitherValues {

  def parseQNameAs(str: String): QName.As[Id] =
    QName.as.parseAll(str).value.mapK(spanToId)

  def parseExpr(str: String): CallArrowExpr[Id] =
    CallArrowExpr.p.parseAll(str).value.mapK(spanToId)

  def parseUse(str: String): UseFromExpr[Id] =
    UseFromExpr.p.parseAll(str).value.mapK(spanToId)

  def parseServiceId(str: String): ServiceIdExpr[Id] =
    ServiceIdExpr.p.parseAll(str).value.mapK(spanToId)

  def parseOn(str: String): OnExpr[Id] =
    OnExpr.p.parseAll(str).value.mapK(spanToId)

  def parseParSeq(str: String): ParSeqExpr[Id] =
    ParSeqExpr.p.parseAll(str).value.mapK(spanToId)

  def parseReturn(str: String): ReturnExpr[Id] =
    ReturnExpr.p.parseAll(str).value.mapK(spanToId)

  def parseAssign(str: String): AssignmentExpr[Id] =
    AssignmentExpr.p.parseAll(str).value.mapK(spanToId)

  def parseVar(str: String): ValueToken[Id] =
    ValueToken.value.parseAll(str).value.mapK(spanToId)

  def parseData(str: String): NamedValueToken[Id] =
    NamedValueToken.dataValue.parseAll(str).value.mapK(spanToId)

  def parseIntoArrow(str: String): PropertyOp[Id] =
    PropertyOp.parseArrow.parseAll(str).value.mapK(spanToId)

  def parsePush(str: String): PushToStreamExpr[Id] =
    PushToStreamExpr.p.parseAll(str).value.mapK(spanToId)

  def parseConstant(str: String): ConstantExpr[Id] =
    ConstantExpr.p.parseAll(str).value.mapK(spanToId)

  def parseService(str: String): ServiceExpr[Id] =
    ServiceExpr.p.parseAll(str).value.mapK(spanToId)

  def parseIf(str: String): IfExpr[Id] =
    IfExpr.p.parseAll(str).value.mapK(spanToId)

  def parseFor(str: String): ForExpr[Id] =
    ForExpr.p.parseAll(str).value.mapK(spanToId)

  def parseElseOtherwise(str: String): ElseOtherwiseExpr[Id] =
    ElseOtherwiseExpr.p.parseAll(str).value.mapK(spanToId)

  def parseFieldType(str: String): FieldTypeExpr[Id] =
    FieldTypeExpr.p.parseAll(str).value.mapK(spanToId)

  def parseAlias(str: String): AliasExpr[Id] =
    AliasExpr.p.parseAll(str).value.mapK(spanToId)

  def parseCollection(str: String): CollectionToken[Id] =
    CollectionToken.collection.parseAll(str).value.mapK(spanToId)

  def parseDataStruct(str: String): DataStructExpr[Id] =
    DataStructExpr.p.parseAll(str).value.mapK(spanToId)

  def parseArrow(str: String): ArrowTypeExpr[Id] =
    ArrowTypeExpr.p.parseAll(str).value.mapK(spanToId)

  def funcExpr(str: String): FuncExpr[Id] = FuncExpr.p.parseAll(str).value.mapK(spanToId)
  def closureExpr(str: String): ClosureExpr[Id] = ClosureExpr.p.parseAll(str).value.mapK(spanToId)
  def arrowExpr(str: String): ArrowExpr[Id] = ArrowExpr.p.parseAll(str).value.mapK(spanToId)

  def prefixToken(value: ValueToken[Id], op: PrefixOp) =
    PrefixToken[Id](value, op)

  def infixToken(left: ValueToken[Id], right: ValueToken[Id], op: InfixOp) =
    InfixToken[Id](left, right, op)

  def mul(left: ValueToken[Id], right: ValueToken[Id]): ValueToken[Id] =
    infixToken(left, right, Mul)

  def sub(left: ValueToken[Id], right: ValueToken[Id]): ValueToken[Id] =
    infixToken(left, right, Sub)

  def div(left: ValueToken[Id], right: ValueToken[Id]): ValueToken[Id] =
    infixToken(left, right, Div)

  def rem(left: ValueToken[Id], right: ValueToken[Id]): ValueToken[Id] =
    infixToken(left, right, Rem)

  def add(left: ValueToken[Id], right: ValueToken[Id]): ValueToken[Id] =
    infixToken(left, right, Add)

  def pow(left: ValueToken[Id], right: ValueToken[Id]): ValueToken[Id] =
    infixToken(left, right, Pow)

  def gt(left: ValueToken[Id], right: ValueToken[Id]): ValueToken[Id] =
    infixToken(left, right, Gt)

  def gte(left: ValueToken[Id], right: ValueToken[Id]): ValueToken[Id] =
    infixToken(left, right, Gte)

  def lt(left: ValueToken[Id], right: ValueToken[Id]): ValueToken[Id] =
    infixToken(left, right, Lt)

  def lte(left: ValueToken[Id], right: ValueToken[Id]): ValueToken[Id] =
    infixToken(left, right, Lte)

  def or(left: ValueToken[Id], right: ValueToken[Id]): ValueToken[Id] =
    infixToken(left, right, Or)

  def and(left: ValueToken[Id], right: ValueToken[Id]): ValueToken[Id] =
    infixToken(left, right, And)

  def not(value: ValueToken[Id]): ValueToken[Id] =
    prefixToken(value, Not)

  def equ(left: ValueToken[Id], right: ValueToken[Id]): ValueToken[Id] =
    infixToken(left, right, Equ)

  def neq(left: ValueToken[Id], right: ValueToken[Id]): ValueToken[Id] =
    infixToken(left, right, Neq)

  val nat = new (Span.S ~> Id) {

    override def apply[A](span: Span.S[A]): A = {
      span._2
    }
  }

  implicit class QueueHelper[T](q: mutable.Queue[T]) {
    def d(): T = q.dequeue()
  }
}
