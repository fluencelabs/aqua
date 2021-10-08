package aqua

import aqua.parser.expr.*
import aqua.parser.expr.func.{
  AbilityIdExpr,
  AssignmentExpr,
  CallArrowExpr,
  ElseOtherwiseExpr,
  ForExpr,
  IfExpr,
  OnExpr,
  PushToStreamExpr,
  ReturnExpr
}
import aqua.parser.lexer.*
import aqua.parser.lift.LiftParser.Implicits.idLiftParser
import aqua.types.LiteralType.{bool, number, string}
import aqua.types.{LiteralType, ScalarType}
import cats.Id
import org.scalatest.EitherValues

import scala.collection.mutable
import scala.language.implicitConversions

object AquaSpec {
  implicit def toAb(str: String): Ability[Id] = Ability[Id](str)

  implicit def toName(str: String): Name[Id] = Name[Id](str)
  implicit def toNameOp(str: Option[String]): Option[Name[Id]] = str.map(s => toName(s))

  implicit def toFields(fields: List[String]): List[IntoField[Id]] =
    fields.map(f => IntoField[Id](f))

  implicit def toVar(name: String): VarLambda[Id] = VarLambda[Id](toName(name), Nil)

  implicit def toVarOp(name: Option[String]): Option[VarLambda[Id]] =
    name.map(s => VarLambda[Id](toName(s), Nil))

  implicit def toVarLambda(name: String, fields: List[String]): VarLambda[Id] =
    VarLambda[Id](toName(name), toFields(fields))
  implicit def toLiteral(name: String, t: LiteralType): Literal[Id] = Literal[Id](name, t)
  implicit def toNumber(n: Int): Literal[Id] = Literal[Id](n.toString, number)
  implicit def toBool(n: Boolean): Literal[Id] = Literal[Id](n.toString, bool)
  implicit def toStr(n: String): Literal[Id] = Literal[Id]("\"" + n + "\"", string)

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

  def parseExpr(str: String): CallArrowExpr[Id] =
    CallArrowExpr.p[Id].parseAll(str).value

  def parseAbId(str: String): AbilityIdExpr[Id] =
    AbilityIdExpr.p[Id].parseAll(str).value

  def parseOn(str: String): OnExpr[Id] =
    OnExpr.p[Id].parseAll(str).value

  def parseReturn(str: String): ReturnExpr[Id] =
    ReturnExpr.p[Id].parseAll(str).value

  def parseAssign(str: String): AssignmentExpr[Id] =
    AssignmentExpr.p[Id].parseAll(str).value

  def parsePush(str: String): PushToStreamExpr[Id] =
    PushToStreamExpr.p[Id].parseAll(str).value

  def parseConstant(str: String): ConstantExpr[Id] =
    ConstantExpr.p[Id].parseAll(str).value

  def parseService(str: String): ServiceExpr[Id] =
    ServiceExpr.p[Id].parseAll(str).value

  def parseIf(str: String): IfExpr[Id] =
    IfExpr.p[Id].parseAll(str).value

  def parseFor(str: String): ForExpr[Id] =
    ForExpr.p[Id].parseAll(str).value

  def parseElse(str: String): ElseOtherwiseExpr[Id] =
    ElseOtherwiseExpr.p[Id].parseAll(str).value

  def parseFieldType(str: String): FieldTypeExpr[Id] =
    FieldTypeExpr.p[Id].parseAll(str).value

  def parseAlias(str: String): AliasExpr[Id] =
    AliasExpr.p[Id].parseAll(str).value

  def parseDataStruct(str: String): DataStructExpr[Id] =
    DataStructExpr.p[Id].parseAll(str).value

  def parseArrow(str: String): ArrowTypeExpr[Id] =
    ArrowTypeExpr.p[Id].parseAll(str).value

  def funcExpr(str: String): FuncExpr[Id] = FuncExpr.p[Id].parseAll(str).value

  implicit class QueueHelper[T](q: mutable.Queue[T]) {
    def d(): T = q.dequeue()
  }
}
