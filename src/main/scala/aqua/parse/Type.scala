package aqua.parse

import aqua.parse.lexer.Token._
import cats.parse.{Parser ⇒ P}

sealed trait Type
sealed trait DataType extends Type
case class ArrayType(data: DataType) extends DataType
case class CustomType(name: String) extends DataType
case class BasicType(name: String) extends DataType

object BasicType {
  private val floatS = "f32" :: "f64" :: Nil
  private val signedS = "s32" :: "s64" :: floatS
  private val numberS = "i32" :: "i64" :: signedS
  private val boolS = "bool" :: Nil
  private val stringS = "string" :: Nil
  private val allS = numberS ++ boolS ++ stringS

  val float = floatS.map(BasicType(_))
  val signed = signedS.map(BasicType(_))
  val number = numberS.map(BasicType(_))
  val bool = boolS.map(BasicType(_))
  val string = stringS.map(BasicType(_))

  val `basictypedef`: P[BasicType] =
    P.oneOf(
      ("()" :: BasicType.allS).map(n ⇒ P.string(n).as(BasicType(n)))
    )
}
case class ArrowType(args: List[DataType], res: DataType) extends Type

object DataType {
  val `customtypedef`: P[CustomType] = `Name`.map(CustomType)

  lazy val `arraytypedef`: P[ArrayType] = (P.string("[]") *> `datatypedef`).map(ArrayType)

  val `datatypedef`: P[DataType] =
    P.oneOf(P.defer(`arraytypedef`) :: BasicType.`basictypedef` :: `customtypedef` :: Nil)
}

object Type {

  val `arrowdef`: P[ArrowType] =
    (comma0(DataType.`datatypedef`).with1 ~ (`->` *> DataType.`datatypedef`)).map {
      case (args, res) ⇒ ArrowType(args, res)
    }

  val `typedef`: P[Type] = P.oneOf(`arrowdef`.backtrack :: DataType.`datatypedef` :: Nil)

}
