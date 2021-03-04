package aqua.parser.lexer

import aqua.parser.lexer.Token._
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser._
import cats.Functor
import cats.parse.{Parser => P}
import cats.syntax.functor._

sealed trait Type[F[_]] extends Token[F]
sealed trait DataType[F[_]] extends Type[F]

// TODO add F[Unit]
case class ArrayType[F[_]](data: DataType[F]) extends DataType[F] {
  override def as[T](v: T)(implicit F: Functor[F]): F[T] = data.as(v)
}

case class CustomType[F[_]](name: F[String]) extends DataType[F] {
  override def as[T](v: T)(implicit F: Functor[F]): F[T] = name.as(v)
}

object CustomType {
  def ct[F[_]: LiftParser]: P[CustomType[F]] = `Name`.lift.map(CustomType(_))
}

case class BasicType[F[_]](name: F[BasicType.Value]) extends DataType[F] {
  override def as[T](v: T)(implicit F: Functor[F]): F[T] = name.as(v)
}

object BasicType {
  case class Value(v: String) extends AnyVal
  private val floatS = "f32" :: "f64" :: Nil
  private val signedS = "s32" :: "s64" :: floatS
  private val numberS = "i32" :: "i64" :: signedS
  private val boolS = "bool" :: Nil
  private val stringS = "string" :: Nil
  private val allS = numberS ++ boolS ++ stringS

  val float = floatS.map(Value)
  val signed = signedS.map(Value)
  val number = numberS.map(Value)
  val bool = boolS.map(Value)
  val string = stringS.map(Value)

  def `basictypedef`[F[_]: LiftParser]: P[BasicType[F]] =
    P.oneOf(
        ("()" :: BasicType.allS).map(n ⇒ P.string(n).as(Value(n)))
      )
      .lift
      .map(BasicType(_))
}

sealed trait ArrowDef[F[_]] {
  def argTypes: List[Type[F]]
  def resType: Option[DataType[F]]
}

case class ArrowType[F[_]](args: List[DataType[F]], res: DataType[F]) extends Type[F] with ArrowDef[F] {
  override def as[T](v: T)(implicit F: Functor[F]): F[T] = (args.headOption getOrElse res).as(v)

  override def argTypes: List[Type[F]] = args

  override def resType: Option[DataType[F]] = Some(res)
}

case class AquaArrowType[F[_]](args: List[Type[F]], res: Option[DataType[F]]) extends ArrowDef[F] {
  override def argTypes: List[Type[F]] = args

  override def resType: Option[DataType[F]] = res
}

object DataType {
  def `arraytypedef`[F[_]: LiftParser]: P[ArrayType[F]] = (P.string("[]") *> `datatypedef`[F]).map(ArrayType(_))

  def `datatypedef`[F[_]: LiftParser]: P[DataType[F]] =
    P.oneOf(P.defer(`arraytypedef`[F]) :: BasicType.`basictypedef`[F] :: CustomType.ct[F] :: Nil)
}

object Type {

  def `arrowdef`[F[_]: LiftParser]: P[ArrowType[F]] =
    (comma0(DataType.`datatypedef`).with1 ~ (`->` *> DataType.`datatypedef`)).map {
      case (args, res) ⇒ ArrowType(args, res)
    }

  def `typedef`[F[_]: LiftParser]: P[Type[F]] = P.oneOf(`arrowdef`.backtrack :: DataType.`datatypedef` :: Nil)

}
