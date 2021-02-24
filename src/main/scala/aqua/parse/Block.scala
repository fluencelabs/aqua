package aqua.parse

import aqua.parse.DataType.`datatypedef`
import aqua.parse.Token._
import aqua.parse.Type.{`arrowdef`, `typedef`}
import cats.data.{NonEmptyList, NonEmptyMap}
import cats.parse.{Parser ⇒ P}


sealed trait Block
case class DefType(name: String, fields: NonEmptyMap[String, DataType]) extends Block
case class DefService(name: String, funcs: NonEmptyMap[String, ArrowType]) extends Block
case class DefFunc(name: String, args: Map[String, Type], body: NonEmptyList[FuncOp], ret: Option[DataType]) extends Block

object DefType {
  val `dname`: P[String] = `data` *> ` ` *> Name <* ` `.? <* `:` <* ` \n`

  val `dataname`: P[(String, DataType)] = (`name` <* ` : `) ~ `datatypedef`
  val `deftype`: P[DefType] =
    (`dname` ~ indented(`dataname` <* ` \n`.?)).map{
      case (n, t) ⇒ DefType(n, t.toNem)
    }
}

object DefFunc {

  val `funcdef`: P[(String, ArrowType)] =
    (`name` <* ` : `) ~ `arrowdef`

  val `funcname`: P[String] = ` `.?.with1 *> `func` *> ` ` *> name <* ` `.?
  val `funcargs`: P[Map[String, Type]] =
    `(` *> comma0((`name` <* ` : `) ~ `typedef`).map(_.toMap) <* `)`
  val `deffunc`: P[DefFunc] =
    (`funcname` ~ (`funcargs` ~ (`->` *> `datatypedef`).? <*  ` : ` <* ` \n`) ~  FuncOp.body).map {
      case ((n, (a, r)), b) ⇒ DefFunc(n, a, b, r)
    }

}

object DefService {
  import DefFunc.`funcdef`

  val `servicename`: P[String] = `service` *> ` ` *> Name <* ` `.? <* `:` <* ` \n`
  val `defservice`: P[DefService] =
    (`servicename` ~ indented(`funcdef` <* ` \n`.?).map(_.toNem)
      ).map{
      case (n, f) ⇒ DefService(n, f)
    }
}

object Block {
  val block: P[Block] = P.oneOf(DefType.`deftype` :: DefService.`defservice` :: DefFunc.`deffunc` :: Nil)
}