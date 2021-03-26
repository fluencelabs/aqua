package aqua

import aqua.parser.expr.{
  AbilityIdExpr,
  AliasExpr,
  ArrowTypeExpr,
  CallArrowExpr,
  DataStructExpr,
  ElseOtherwiseExpr,
  FieldTypeExpr,
  ForExpr,
  FuncExpr,
  IfExpr,
  OnExpr,
  ParExpr,
  ReturnExpr,
  ServiceExpr
}
import aqua.parser.lexer.{
  Ability,
  Arg,
  ArrayTypeToken,
  ArrowTypeToken,
  BasicTypeToken,
  CustomTypeToken,
  DataTypeToken,
  EqOp,
  IntoField,
  Literal,
  Name,
  TypeToken,
  VarLambda
}
import cats.Id
import aqua.parser.lift.LiftParser.Implicits.idLiftParser
import aqua.semantics.LiteralType.{bool, number, string}
import aqua.semantics.{LiteralType, ScalarType}
import org.scalatest.EitherValues

import scala.language.implicitConversions

object Utils {
  implicit def toAb(str: String): Ability[Id] = Ability[Id](str)

  implicit def toName(str: String): Name[Id] = Name[Id](str)

  implicit def toFields(fields: List[String]): List[IntoField[Id]] = fields.map(f => IntoField[Id](f))

  implicit def toVar(name: String): VarLambda[Id] = VarLambda[Id](toName(name), Nil)

  implicit def toVarLambda(name: String, fields: List[String]): VarLambda[Id] =
    VarLambda[Id](toName(name), toFields(fields))
  implicit def toLiteral(name: String, t: LiteralType): Literal[Id] = Literal[Id](name, t)
  implicit def toNumber(n: Int): Literal[Id] = Literal[Id](n.toString, number)
  implicit def toBool(n: Boolean): Literal[Id] = Literal[Id](n.toString, bool)
  implicit def toStr(n: String): Literal[Id] = Literal[Id]("\"" + n + "\"", string)

  implicit def toCustomType(str: String): CustomTypeToken[Id] = CustomTypeToken[Id](str)
  def toArrayType(str: String): ArrayTypeToken[Id] = ArrayTypeToken[Id]((), str)

  implicit def toArrowType(args: List[DataTypeToken[Id]], res: Option[DataTypeToken[Id]]): ArrowTypeToken[Id] =
    ArrowTypeToken[Id]((), args, res)

  implicit def toCustomArg(str: String, customType: String): Arg[Id] = Arg[Id](str, toCustomType(customType))

  implicit def toArg(str: String, typeToken: TypeToken[Id]): Arg[Id] = Arg[Id](str, typeToken)

  implicit def scToBt(sc: ScalarType): BasicTypeToken[Id] = BasicTypeToken[Id](sc)
}

trait Utils extends EitherValues {

  def parseExpr(str: String): CallArrowExpr[Id] =
    CallArrowExpr.p[Id].parseAll(str).value

  def parseAbId(str: String): AbilityIdExpr[Id] =
    AbilityIdExpr.p[Id].parseAll(str).value

  def parseOn(str: String): OnExpr[Id] =
    OnExpr.p[Id].parseAll(str).value

  def parsePar(str: String): ParExpr[Id] =
    ParExpr.p[Id].parseAll(str).value

  def parseReturn(str: String): ReturnExpr[Id] =
    ReturnExpr.p[Id].parseAll(str).value

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
}
