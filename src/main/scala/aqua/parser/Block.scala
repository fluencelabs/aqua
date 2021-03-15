package aqua.parser

import aqua.parser.lexer.DataTypeToken.`datatypedef`
import aqua.parser.lexer.Token._
import aqua.parser.lexer.TypeToken.`typedef`
import aqua.parser.lexer.{Ability, AquaArrowType, ArrowTypeToken, CustomTypeToken, DataTypeToken, Name, TypeToken}
import aqua.parser.lift.LiftParser
import aqua.parser.lift.LiftParser._
import cats.Comonad
import cats.data.{NonEmptyList, NonEmptyMap}
import cats.parse.{Parser => P, Parser0 => P0}
import cats.syntax.comonad._
import shapeless.HNil

sealed trait Block[F[_], L] extends Expression[F, L]

case class DefType[F[_], L](
  name: CustomTypeToken[F],
  fields: NonEmptyMap[String, (Name[F], DataTypeToken[F])],
  context: L
) extends Block[F, L]

case class DefService[F[_], L](name: Ability[F], funcs: NonEmptyMap[String, ArrowTypeToken[F]], context: L)
    extends Block[F, L]

// TODO arg is either Var, or ArrowName
case class FuncHead[F[_]](
  name: Name[F],
  args: List[(String, F[String], TypeToken[F])],
  ret: Option[DataTypeToken[F]]
) {

  def arrowDef: AquaArrowType[F] =
    AquaArrowType(args.map(_._3), ret)
}

case class DefFunc[F[_], L](head: FuncHead[F], body: NonEmptyList[FuncExpr[F, L]], context: L) extends Block[F, L]

case class DefAlias[F[_], L](alias: CustomTypeToken[F], target: TypeToken[F], context: L) extends Block[F, L]

object DefType {

  def `dname`[F[_]: LiftParser: Comonad]: P[CustomTypeToken[F]] =
    `data` *> ` ` *> CustomTypeToken.ct[F] <* ` `.? <* `:` <* ` \n+`

  def `dataname`[F[_]: LiftParser: Comonad](indent: String): P[(Name[F], DataTypeToken[F])] =
    (Name.p[F] <* ` : `) ~ `datatypedef`

  def `deftype`[F[_]: LiftParser: Comonad]: P[DefType[F, HNil]] =
    (`dname` ~ indented(`dataname`(_), "")).map {
      case (n, t) ⇒ DefType(n, t.map(kv => kv._1.name.extract -> kv).toNem, HNil)
    }
}

object DefFunc {

  def `funcname`[F[_]: LiftParser: Comonad]: P[Name[F]] = ` `.?.with1 *> `func` *> ` ` *> Name.p <* ` `.?

  def `funcargs`[F[_]: LiftParser: Comonad]: P[List[(String, F[String], TypeToken[F])]] =
    `(` *> comma0((`name`.lift <* ` : `) ~ `typedef`).map(_.map(kv => (kv._1.extract, kv._1, kv._2))) <* `)`

  def `funchead`[F[_]: LiftParser: Comonad]: P[FuncHead[F]] =
    (`funcname` ~ (`funcargs` ~ (`->` *> `datatypedef`).?)).map {
      case (n, (a, r)) ⇒ FuncHead(n, a, r)
    }

  // TODO: if funchead has return type, for the last op, add extract, add Return.reply(extracted)
  def `deffunc`[F[_]: LiftParser: Comonad]: P[DefFunc[F, HNil]] =
    ((`funchead` <* ` : ` <* ` \n+`) ~ FuncExpr.body).map {
      case (h, b) ⇒ DefFunc(h, b, HNil)
    }

}

object DefService {

  // TODO use name's [F] for ArrowName
  def `funcdef`[F[_]: LiftParser: Comonad]: P[(String, ArrowTypeToken[F])] =
    (`name` <* ` : `) ~ ArrowTypeToken.`arrowdef`

  def `servicename`[F[_]: LiftParser: Comonad]: P[Ability[F]] =
    `service` *> ` ` *> Ability.ab[F] <* ` `.? <* `:` <* ` \n+`

  // TODO switch to funchead?
  def `defservice`[F[_]: LiftParser: Comonad]: P[DefService[F, HNil]] =
    (`servicename` ~ indented(_ => `funcdef`, "").map(_.toNem)).map {
      case (n, f) ⇒ DefService(n, f, HNil)
    }
}

object DefAlias {

  def `defalias`[F[_]: LiftParser: Comonad]: P[DefAlias[F, HNil]] =
    ((`alias` *> ` ` *> CustomTypeToken.ct[F] <* ` : `) ~ `typedef`).map {
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

  def blocks[F[_]: LiftParser: Comonad]: P0[List[Block[F, HNil]]] =
    P.repSep0(block[F], ` \n+`) <* ` \n+`.?
}
