package aqua.parse

import aqua.parse.Token._
import cats.parse.{Parser ⇒ P}

sealed trait Type
sealed trait DataType extends Type
case class ArrayType(data: DataType) extends DataType
case class CustomType(name: String) extends DataType
case class BasicType(name: String) extends DataType
case class ArrowType(args: List[DataType], res: DataType) extends Type

object DataType {
  val `customtypedef`: P[CustomType] = `Name`.map(CustomType)
  val `basictypedef`: P[BasicType] =
    P.oneOf(
      ("i32" :: "i64" :: Nil).map(n ⇒ P.string(n).as(BasicType(n)))
    )
  lazy val `arraytypedef`: P[ArrayType] = (P.string("[]") *> `datatypedef`).map(ArrayType)

  val `datatypedef`: P[DataType] = P.oneOf( P.defer(`arraytypedef`) :: `basictypedef` :: `customtypedef` :: Nil)
}

object Type {

  val `arrowdef`: P[ArrowType] =
    (comma0(DataType.`datatypedef`).with1 ~ (`->` *> DataType.`datatypedef`))
      .map{case (args, res) ⇒ ArrowType(args, res)}

  val `typedef`: P[Type] = P.oneOf(DataType.`datatypedef` :: `arrowdef` :: Nil)

}