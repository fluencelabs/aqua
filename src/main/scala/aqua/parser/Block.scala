package aqua.parser

import aqua.parser.lexer.DataType.`datatypedef`
import aqua.parser.lexer.Token._
import aqua.parser.lexer.Type.{`arrowdef`, `typedef`}
import aqua.parser.lexer.{Ability, AquaArrowType, ArrowName, ArrowType, CustomType, DataType, Token, Type}
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser._
import cats.{Comonad, Functor}
import cats.data.{NonEmptyList, NonEmptyMap}
import cats.parse.{Parser => P}
import cats.syntax.comonad._

sealed trait Block[F[_]] extends Token[F]

case class DefType[F[_]](name: CustomType[F], fields: NonEmptyMap[String, (F[String], DataType[F])]) extends Block[F] {
  override def as[T](v: T)(implicit F: Functor[F]): F[T] = name.as(v)
}

case class DefService[F[_]](name: Ability[F], funcs: NonEmptyMap[String, ArrowType[F]]) extends Block[F] {
  override def as[T](v: T)(implicit F: Functor[F]): F[T] = name.as(v)
}

case class FuncHead[F[_]](name: ArrowName[F], args: List[(String, F[String], Type[F])], ret: Option[DataType[F]]) {

  def toArrowDef(implicit F: Comonad[F]): F[AquaArrowType[F]] =
    name.as(AquaArrowType(args.map(_._3), ret))
}

case class DefFunc[F[_]](head: FuncHead[F], body: NonEmptyList[FuncOp[F]]) extends Block[F] {
  override def as[T](v: T)(implicit F: Functor[F]): F[T] = head.name.as(v)
}

case class DefAlias[F[_]](alias: CustomType[F], target: Type[F]) extends Block[F] {
  override def as[T](v: T)(implicit F: Functor[F]): F[T] = alias.as(v)
}

object DefType {
  def `dname`[F[_]: LiftParser]: P[CustomType[F]] = `data` *> ` ` *> CustomType.ct[F] <* ` `.? <* `:` <* ` \n*`

  def `dataname`[F[_]: LiftParser]: P[(F[String], DataType[F])] =
    (`name`.lift <* ` : `) ~ `datatypedef`

  def `deftype`[F[_]: LiftParser: Comonad]: P[DefType[F]] =
    (`dname` ~ indented(`dataname`)).map {
      case (n, t) ⇒ DefType(n, t.map(kv => kv._1.extract -> kv).toNem)
    }
}

object DefFunc {

  def `funcdef`[F[_]: LiftParser]: P[(String, ArrowType[F])] =
    (`name` <* ` : `) ~ `arrowdef`

  def `funcname`[F[_]: LiftParser]: P[ArrowName[F]] = ` `.?.with1 *> `func` *> ` ` *> ArrowName.an <* ` `.?

  def `funcargs`[F[_]: LiftParser: Comonad]: P[List[(String, F[String], Type[F])]] =
    `(` *> comma0((`name`.lift <* ` : `) ~ `typedef`).map(_.map(kv => (kv._1.extract, kv._1, kv._2))) <* `)`

  def `funchead`[F[_]: LiftParser: Comonad]: P[FuncHead[F]] =
    (`funcname` ~ (`funcargs` ~ (`->` *> `datatypedef`).?)).map {
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

  def `servicename`[F[_]: LiftParser]: P[Ability[F]] = `service` *> ` ` *> Ability.ab[F] <* ` `.? <* `:` <* ` \n*`

  // TODO switch to funchead?
  def `defservice`[F[_]: LiftParser]: P[DefService[F]] =
    (`servicename` ~ indented(`funcdef`).map(_.toNem)).map {
      case (n, f) ⇒ DefService(n, f)
    }
}

object DefAlias {

  def `defalias`[F[_]: LiftParser]: P[DefAlias[F]] =
    ((`alias` *> ` ` *> CustomType.ct[F] <* ` : `) ~ `typedef`).map {
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
