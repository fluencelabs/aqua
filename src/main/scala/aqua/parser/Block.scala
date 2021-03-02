package aqua.parser

import aqua.parser.lexer.DataType.`datatypedef`
import aqua.parser.lexer.Token._
import aqua.parser.lexer.Type.{`arrowdef`, `typedef`}
import aqua.parser.lexer.{Ability, AquaArrowType, ArrowName, ArrowType, CustomType, DataType, Type, Var}
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser._
import cats.Comonad
import cats.data.{NonEmptyList, NonEmptyMap}
import cats.parse.{Parser => P}
import cats.syntax.comonad._
import shapeless.HNil

sealed trait Block[F[_], L] extends Expression[F, L]

case class DefType[F[_], L](name: CustomType[F], fields: NonEmptyMap[String, (Var[F], DataType[F])], context: L)
    extends Block[F, L]

case class DefService[F[_], L](name: Ability[F], funcs: NonEmptyMap[String, ArrowType[F]], context: L)
    extends Block[F, L]

// TODO arg is either Var, or ArrowName
case class FuncHead[F[_]](name: ArrowName[F], args: List[(String, F[String], Type[F])], ret: Option[DataType[F]]) {

  def toArrowDef(implicit F: Comonad[F]): F[AquaArrowType[F]] =
    name.as(AquaArrowType(args.map(_._3), ret))
}

case class DefFunc[F[_], L](head: FuncHead[F], body: NonEmptyList[FuncOp[F, L]], context: L) extends Block[F, L]

case class DefAlias[F[_], L](alias: CustomType[F], target: Type[F], context: L) extends Block[F, L]

object DefType {
  def `dname`[F[_]: LiftParser]: P[CustomType[F]] = `data` *> ` ` *> CustomType.ct[F] <* ` `.? <* `:` <* ` \n+`

  def `dataname`[F[_]: LiftParser]: P[(Var[F], DataType[F])] =
    (Var.v[F] <* ` : `) ~ `datatypedef`

  def `deftype`[F[_]: LiftParser: Comonad]: P[DefType[F, HNil]] =
    (`dname` ~ indented(`dataname`)).map {
      case (n, t) ⇒ DefType(n, t.map(kv => kv._1.name.extract -> kv).toNem, HNil)
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
  def `deffunc`[F[_]: LiftParser: Comonad]: P[DefFunc[F, HNil]] =
    ((`funchead` <* ` : ` <* ` \n+`) ~ FuncOp.body).map {
      case (h, b) ⇒ DefFunc(h, b, HNil)
    }

}

object DefService {
  import DefFunc.`funcdef`

  def `servicename`[F[_]: LiftParser]: P[Ability[F]] = `service` *> ` ` *> Ability.ab[F] <* ` `.? <* `:` <* ` \n+`

  // TODO switch to funchead?
  def `defservice`[F[_]: LiftParser]: P[DefService[F, HNil]] =
    (`servicename` ~ indented(`funcdef`).map(_.toNem)).map {
      case (n, f) ⇒ DefService(n, f, HNil)
    }
}

object DefAlias {

  def `defalias`[F[_]: LiftParser]: P[DefAlias[F, HNil]] =
    ((`alias` *> ` ` *> CustomType.ct[F] <* ` : `) ~ `typedef`).map {
      case (ct, t) => DefAlias(ct, t, HNil)
    }
}

object Block {

  def block[F[_]: LiftParser: Comonad]: P[Block[F, HNil]] =
    ` \n+`.rep0.with1 *> P.oneOf(
      DefType.`deftype` ::
        DefService.`defservice` ::
        DefFunc.`deffunc` ::
        DefAlias.`defalias` ::
        Nil
    )
}
