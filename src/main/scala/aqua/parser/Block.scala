package aqua.parser

import aqua.parser.DataType.{`customtypedef`, `datatypedef`}
import aqua.parser.lexer.Token._
import aqua.parser.Type.{`arrowdef`, `typedef`}
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser._
import cats.Comonad
import cats.data.{NonEmptyList, NonEmptyMap}
import cats.parse.{Parser => P}
import cats.syntax.comonad._

sealed trait Block[F[_]]
case class DefType[F[_]](name: F[String], fields: NonEmptyMap[String, (F[String], F[DataType])]) extends Block[F]
case class DefService[F[_]](name: F[String], funcs: NonEmptyMap[String, ArrowType]) extends Block[F]

case class FuncHead[F[_]](name: F[String], args: Map[String, (F[String], F[Type])], ret: Option[F[DataType]])

case class DefFunc[F[_]](head: FuncHead[F], body: NonEmptyList[F[FuncOp[F]]]) extends Block[F]
case class DefAlias[F[_]](alias: CustomType, target: Type) extends Block[F]

object DefType {
  def `dname`[F[_]: LiftParser]: P[F[String]] = `data` *> ` ` *> Name.lift <* ` `.? <* `:` <* ` \n*`

  def `dataname`[F[_]: LiftParser]: P[(F[String], F[DataType])] = (`name`.lift <* ` : `) ~ `datatypedef`.lift

  def `deftype`[F[_]: LiftParser: Comonad]: P[DefType[F]] =
    (`dname` ~ indented(`dataname`)).map {
      case (n, t) ⇒ DefType(n, t.map(kv => kv._1.extract -> kv).toNem)
    }
}

object DefFunc {

  val `funcdef`: P[(String, ArrowType)] =
    (`name` <* ` : `) ~ `arrowdef`

  def `funcname`[F[_]: LiftParser]: P[F[String]] = ` `.?.with1 *> `func` *> ` ` *> name.lift <* ` `.?

  def `funcargs`[F[_]: LiftParser: Comonad]: P[Map[String, (F[String], F[Type])]] =
    `(` *> comma0((`name`.lift <* ` : `) ~ `typedef`.lift).map(_.map(kv => kv._1.extract -> kv).toMap) <* `)`

  def `funchead`[F[_]: LiftParser: Comonad]: P[FuncHead[F]] =
    (`funcname` ~ (`funcargs` ~ (`->` *> `datatypedef`.lift).?)).map {
      case (n, (a, r)) ⇒ FuncHead(n, a, r)
    }

  // TODO: if funchead has return type, for the last op, add extract, add Return.reply(extracted)
  def `deffunc`[F[_]: LiftParser: Comonad]: P[DefFunc[F]] =
    ((`funchead` <* ` : ` <* ` \n*`) ~ FuncOp.body).map {
      case (h, b) ⇒ DefFunc(h, b)
    }

}

object DefService {
  import DefFunc.`funcdef`

  def `servicename`[F[_]: LiftParser]: P[F[String]] = `service` *> ` ` *> Name.lift <* ` `.? <* `:` <* ` \n*`

  // TODO switch to funchead?
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

  def block[F[_]: LiftParser: Comonad]: P[Block[F]] =
    ` \n*`.rep0.with1 *> P.oneOf(
      DefType.`deftype` ::
        DefService.`defservice` ::
        DefFunc.`deffunc` ::
        DefAlias.`defalias` ::
        Nil
    )
}
