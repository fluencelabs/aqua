package aqua

import aqua.parser.expr.*
import aqua.parser.expr.func.{AbilityIdExpr, ArrowExpr, AssignmentExpr, CallArrowExpr, ClosureExpr, ElseOtherwiseExpr, ForExpr, IfExpr, OnExpr, PushToStreamExpr, ReturnExpr}
import aqua.parser.lexer.*
import aqua.parser.lift.LiftParser.Implicits.idLiftParser
import aqua.types.LiteralType.{bool, number, string}
import aqua.types.{LiteralType, ScalarType}
import cats.{Id, ~>}
import org.scalatest.EitherValues
import aqua.parser.lift.Span
import aqua.parser.lift.Span.{P0ToSpan, PToSpan}

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

  def parseExpr(str: String): CallArrowExpr[Span.F] =
    CallArrowExpr.p.parseAll(str).value

  def parseAbId(str: String): AbilityIdExpr[Span.F] =
    AbilityIdExpr.p.parseAll(str).value

  def parseOn(str: String): OnExpr[Span.F] =
    OnExpr.p.parseAll(str).value

  def parseReturn(str: String): ReturnExpr[Span.F] =
    ReturnExpr.p.parseAll(str).value

  def parseAssign(str: String): AssignmentExpr[Span.F] =
    AssignmentExpr.p.parseAll(str).value

  def parsePush(str: String): PushToStreamExpr[Span.F] =
    PushToStreamExpr.p.parseAll(str).value

  def parseConstant(str: String): ConstantExpr[Span.F] =
    ConstantExpr.p.parseAll(str).value

  def parseService(str: String): ServiceExpr[Span.F] =
    ServiceExpr.p.parseAll(str).value

  def parseIf(str: String): IfExpr[Span.F] =
    IfExpr.p.parseAll(str).value

  def parseFor(str: String): ForExpr[Span.F] =
    ForExpr.p.parseAll(str).value

  def parseElse(str: String): ElseOtherwiseExpr[Span.F] =
    ElseOtherwiseExpr.p.parseAll(str).value

  def parseFieldType(str: String): FieldTypeExpr[Span.F] =
    FieldTypeExpr.p.parseAll(str).value

  def parseAlias(str: String): AliasExpr[Span.F] =
    AliasExpr.p.parseAll(str).value

  def parseDataStruct(str: String): DataStructExpr[Span.F] =
    DataStructExpr.p.parseAll(str).value

  def parseArrow(str: String): ArrowTypeExpr[Span.F] =
    ArrowTypeExpr.p.parseAll(str).value

  def funcExpr(str: String): FuncExpr[Span.F] = FuncExpr.p.parseAll(str).value
  def closureExpr(str: String): ClosureExpr[Span.F] = ClosureExpr.p.parseAll(str).value
  def arrowExpr(str: String): ArrowExpr[Span.F] = ArrowExpr.p.parseAll(str).value


  val nat = new (Span.F ~> Id) {
    override def apply[A](span: Span.F[A]): A = {
      span._2
    }
  }

  implicit class QueueHelper[T](q: mutable.Queue[T]) {
    def d(): T = q.dequeue()
  }
}
