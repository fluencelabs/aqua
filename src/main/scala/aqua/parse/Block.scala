package aqua.parse

import aqua.parse.DataType.{`customtypedef`, `datatypedef`}
import aqua.parse.lexer.Token._
import aqua.parse.Type.{`arrowdef`, `typedef`}
import aqua.parse.lift.LiftParser
import aqua.parse.lift.LiftParser._
import cats.Functor
import cats.data.{NonEmptyList, NonEmptyMap}
import cats.parse.{Parser => P}

sealed trait Block[F[_]]
case class DefType[F[_]](name: F[String], fields: NonEmptyMap[String, F[DataType]]) extends Block[F]
case class DefService[F[_]](name: F[String], funcs: NonEmptyMap[String, ArrowType]) extends Block[F]

case class FuncHead[F[_]](name: F[String], args: Map[String, Type], ret: Option[DataType])

case class DefFunc[F[_]](head: FuncHead[F], body: NonEmptyList[F[FuncOp[F]]]) extends Block[F]
case class DefAlias[F[_]](alias: CustomType, target: Type) extends Block[F]

object DefType {
  def `dname`[F[_]: LiftParser]: P[F[String]] = `data` *> ` ` *> Name.lift <* ` `.? <* `:` <* ` \n*`

  def `dataname`[F[_]: LiftParser]: P[(String, F[DataType])] = (`name` <* ` : `) ~ `datatypedef`.lift

  def `deftype`[F[_]: LiftParser]: P[DefType[F]] =
    (`dname` ~ indented(`dataname`)).map {
      case (n, t) ⇒ DefType(n, t.toNem)
    }
}

object DefFunc {

  val `funcdef`: P[(String, ArrowType)] =
    (`name` <* ` : `) ~ `arrowdef`

  def `funcname`[F[_]: LiftParser]: P[F[String]] = ` `.?.with1 *> `func` *> ` ` *> name.lift <* ` `.?

  val `funcargs`: P[Map[String, Type]] =
    `(` *> comma0((`name` <* ` : `) ~ `typedef`).map(_.toMap) <* `)`

  def `funchead`[F[_]: LiftParser]: P[FuncHead[F]] =
    (`funcname` ~ (`funcargs` ~ (`->` *> `datatypedef`).?)).map {
      case (n, (a, r)) ⇒ FuncHead(n, a, r)
    }

  def `deffunc`[F[_]: LiftParser: Functor]: P[DefFunc[F]] =
    ((`funchead` <* ` : ` <* ` \n*`) ~ FuncOp.body).map {
      case (h, b) ⇒ DefFunc(h, b)
    }

}

object DefService {
  import DefFunc.`funcdef`

  def `servicename`[F[_]: LiftParser]: P[F[String]] = `service` *> ` ` *> Name.lift <* ` `.? <* `:` <* ` \n*`

  def `defservice`[F[_]: LiftParser]: P[DefService[F]] =
    (`servicename` ~ indented(`funcdef`).map(_.toNem)).map {
      case (n, f) ⇒ DefService(n, f)
    }
}

object DefAlias {

  def `defalias`[F[_]: LiftParser]: P[DefAlias[F]] =
    ((`alias` *> ` ` *> `customtypedef` <* ` : `) ~ `typedef`).map {
      case (ct, t) => DefAlias(ct, t)
    }
}

object Block {

  def block[F[_]: LiftParser: Functor]: P[Block[F]] =
    ` \n*`.rep0.with1 *> P.oneOf(
      DefType.`deftype` :: DefService.`defservice` :: DefFunc.`deffunc` :: DefAlias.`defalias` :: Nil
    )
}
