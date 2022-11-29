package aqua

import aqua.AquaSpec.spanToId
import aqua.parser.expr.*
import aqua.parser.expr.func.{AbilityIdExpr, ArrowExpr, AssignmentExpr, CallArrowExpr, ClosureExpr, ElseOtherwiseExpr, ForExpr, IfExpr, OnExpr, PushToStreamExpr, ReturnExpr}
import aqua.parser.head.FromExpr.NameOrAbAs
import aqua.parser.head.{FromExpr, UseFromExpr}
import aqua.parser.lexer.*
import aqua.parser.lexer.Token.LiftToken
import aqua.parser.lift.LiftParser.Implicits.idLiftParser
import aqua.types.LiteralType.{bool, number, string}
import aqua.types.{LiteralType, ScalarType}
import cats.{Id, ~>}
import org.scalatest.EitherValues
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan, PToSpan}
import cats.~>
import cats.syntax.bifunctor.*

import scala.collection.mutable
import scala.language.implicitConversions

object AquaSpec {

  def spanToId: Span.S ~> Id = new (Span.S ~> Id) {

    override def apply[A](span: Span.S[A]): Id[A] = {
      span._2
    }
  }

  implicit def toAb(str: String): Ability[Id] = Ability[Id](str)

  implicit def toName(str: String): Name[Id] = Name[Id](str)
  implicit def toNameOp(str: Option[String]): Option[Name[Id]] = str.map(s => toName(s))

  implicit def toFields(fields: List[String]): List[IntoField[Id]] =
    fields.map(f => IntoField[Id](f))

  implicit def toVar(name: String): VarToken[Id] = VarToken[Id](toName(name), Nil)

  implicit def toVarOp(name: Option[String]): Option[VarToken[Id]] =
    name.map(s => VarToken[Id](toName(s), Nil))

  implicit def toVarLambda(name: String, fields: List[String]): VarToken[Id] =
    VarToken[Id](toName(name), toFields(fields))

  implicit def toVarIndex(name: String, idx: Int): VarToken[Id] =
    VarToken[Id](toName(name), IntoIndex[Id](toNumber(idx).unit, Some(toNumber(idx))) :: Nil)
  implicit def toLiteral(name: String, t: LiteralType): LiteralToken[Id] = LiteralToken[Id](name, t)
  implicit def toNumber(n: Int): LiteralToken[Id] = LiteralToken[Id](n.toString, number)
  implicit def toBool(n: Boolean): LiteralToken[Id] = LiteralToken[Id](n.toString, bool)
  implicit def toStr(n: String): LiteralToken[Id] = LiteralToken[Id]("\"" + n + "\"", string)

  implicit def toCustomType(str: String): CustomTypeToken[Id] = CustomTypeToken[Id](str)
  def toArrayType(str: String): ArrayTypeToken[Id] = ArrayTypeToken[Id]((), str)

  implicit def toArrowType(
    args: List[DataTypeToken[Id]],
    res: Option[DataTypeToken[Id]]
  ): ArrowTypeToken[Id] =
    ArrowTypeToken[Id]((), args.map(None -> _), res.toList)

  def toNamedArrow(
    args: List[(String, TypeToken[Id])],
    res: List[DataTypeToken[Id]]
  ): ArrowTypeToken[Id] =
    ArrowTypeToken[Id]((), args.map(ab => Some(Name[Id](ab._1)) -> ab._2), res)

  implicit def toCustomArg(str: String, customType: String): Arg[Id] =
    Arg[Id](str, toCustomType(customType))

  implicit def toArg(str: String, typeToken: TypeToken[Id]): Arg[Id] = Arg[Id](str, typeToken)

  implicit def toArgSc(str: String, scalarType: ScalarType): Arg[Id] =
    Arg[Id](str, scToBt(scalarType))

  implicit def scToBt(sc: ScalarType): BasicTypeToken[Id] = BasicTypeToken[Id](sc)

  val boolSc: BasicTypeToken[Id] = BasicTypeToken[Id](ScalarType.bool)
  val stringSc: BasicTypeToken[Id] = BasicTypeToken[Id](ScalarType.string)
}

trait AquaSpec extends EitherValues {

  def fromExprToId(fromExpr: NameOrAbAs[Span.S]): NameOrAbAs[Id] =
    fromExpr.bimap(
      a => (a._1.mapK(spanToId), a._2.map(_.mapK(spanToId))),
      a => (a._1.mapK(spanToId), a._2.map(_.mapK(spanToId)))
    )

  def parseNameOrAbs(str: String): NameOrAbAs[Id] =
    fromExprToId(FromExpr.nameOrAbAs.parseAll(str).value)

  def parseExpr(str: String): CallArrowExpr[Id] =
    CallArrowExpr.p.parseAll(str).value.mapK(spanToId)

  def parseUse(str: String): UseFromExpr[Id] =
    UseFromExpr.p.parseAll(str).value.mapK(spanToId)

  def parseAbId(str: String): AbilityIdExpr[Id] =
    AbilityIdExpr.p.parseAll(str).value.mapK(spanToId)

  def parseOn(str: String): OnExpr[Id] =
    OnExpr.p.parseAll(str).value.mapK(spanToId)

  def parseReturn(str: String): ReturnExpr[Id] =
    ReturnExpr.p.parseAll(str).value.mapK(spanToId)

  def parseAssign(str: String): AssignmentExpr[Id] =
    AssignmentExpr.p.parseAll(str).value.mapK(spanToId)

  def parseData(str: String): DataValueToken[Id] =
    DataValueToken.dataValue.parseAll(str).value.mapK(spanToId)

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

  def parseElse(str: String): ElseOtherwiseExpr[Id] =
    ElseOtherwiseExpr.p.parseAll(str).value.mapK(spanToId)

  def parseFieldType(str: String): FieldTypeExpr[Id] =
    FieldTypeExpr.p.parseAll(str).value.mapK(spanToId)

  def parseAlias(str: String): AliasExpr[Id] =
    AliasExpr.p.parseAll(str).value.mapK(spanToId)

  def parseDataStruct(str: String): DataStructExpr[Id] =
    DataStructExpr.p.parseAll(str).value.mapK(spanToId)

  def parseArrow(str: String): ArrowTypeExpr[Id] =
    ArrowTypeExpr.p.parseAll(str).value.mapK(spanToId)

  def funcExpr(str: String): FuncExpr[Id] = FuncExpr.p.parseAll(str).value.mapK(spanToId)
  def closureExpr(str: String): ClosureExpr[Id] = ClosureExpr.p.parseAll(str).value.mapK(spanToId)
  def arrowExpr(str: String): ArrowExpr[Id] = ArrowExpr.p.parseAll(str).value.mapK(spanToId)

  val nat = new (Span.S ~> Id) {

    override def apply[A](span: Span.S[A]): A = {
      span._2
    }
  }

  implicit class QueueHelper[T](q: mutable.Queue[T]) {
    def d(): T = q.dequeue()
  }
}
