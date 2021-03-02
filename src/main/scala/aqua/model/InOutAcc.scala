package aqua.model

import aqua.parser.{
  AbilityFuncCall,
  AbilityId,
  AbilityResolve,
  Block,
  DefAlias,
  DefFunc,
  DefService,
  DefType,
  Expression,
  Extract,
  FuncCall,
  FuncOp,
  On,
  Par
}
import aqua.parser.lexer.{Ability, ArrowName, ArrowType, CustomType, DataType, Token, Value, Var}
import aqua.model.marker.{ArrowMarker, FuncArrow, LocalArrow, Marker, TypeAlias, TypeDef, TypeMarker}
import cats.{Comonad, Functor}
import cats.data.NonEmptyList
import cats.syntax.comonad._
import cats.syntax.functor._

case class InOutAcc[F[_], In <: Token[F], Out <: Marker[F, _]](
  in: Acc[F, In],
  out: Acc[F, Out]
) {
  type Self = InOutAcc[F, In, Out]

  def combine(other: Self, mode: Option[Mode])(implicit F: Comonad[F]): Self =
    mode match {
      case None => combineSeq(other)
      case Some(XorMode) => combineXor(other)
      case Some(ParMode) => combinePar(other)
    }

  def combineSeq(other: Self): Self =
    copy(in = in.add(other.in, out.keys), out = out add other.out)

  def combinePar(other: Self): Self =
    copy(in = in add other.in, out = out add other.out)

  def combineXor(other: Self): Self =
    copy(in = in add other.in)

  def addIn(addition: Acc[F, In]): Self =
    copy(in = in add addition)

  def subIn(rem: String): Self =
    copy(in = in sub rem)

  def addOut(addition: Acc[F, Out]): Self =
    copy(out = out add addition)

  def collectOut(pf: PartialFunction[Out, Out]): Self =
    copy(out = out.copy(data = out.data.map {
      case (k, v) => k -> v.toList.collect(pf)
    }.collect {
      case (k, h :: tail) => k -> NonEmptyList[Out](h, tail)
    }))

  def subOut(rem: String): Self =
    copy(out = out sub rem)

  def eraseOut: Self = copy(out = out.erase)
  def eraseIn: Self = copy(in = in.erase)
}

object InOutAcc {

  def empty[F[_], In <: Token[F], Out <: Marker[F, _]]: InOutAcc[F, In, Out] =
    InOutAcc(Acc.empty[F, In], Acc.empty[F, Out])

  /*
  type Abilities[F[_]] = InOutAcc[F, Ability[F], DefService[F]]

  object Abilities extends Visitor[Abilities] {

    def funcOp[F[_]: Comonad](op: FuncOp[F]): Abilities[F] =
      op match {
        case ar: AbilityResolve[F] =>
          (empty: Abilities[F]) addIn Acc.one(ar.ability.name.extract, ar.ability)
        case Par(p, op) =>
          funcOp(op).par(p)
        case On(p, ops) =>
          ops
            .widen[FuncOp[F]]
            .map(funcOp[F](_).on(p))
            .reduceLeft(_ combine _)
        case _ =>
          empty: Abilities[F]
      }

    // No notion for abilities in funcdef yet
    override def func[F[_]: Comonad](func: DefFunc[F]): Abilities[F] =
      func.body.map(funcOp[F]).reduceLeft(_ combine _).unsetScope.eraseOut

    override def block[F[_]: Comonad](block: Block[F]): Abilities[F] =
      block match {
        case fn: DefFunc[F] =>
          func(fn)
        case defService: DefService[F] =>
          (empty: Abilities[F])
            .addOut(Acc.one(defService.name.name.extract, defService))
        case _ =>
          empty: Abilities[F]
      }
  }

  type Types[F[_]] = InOutAcc[F, CustomType[F], TypeMarker[F]]

  object Types extends Visitor[Types] {

    override def funcOp[F[_]: Comonad](op: FuncOp[F]): Types[F] =
      op match {
        case _ => empty: Types[F]
      }

    override def func[F[_]: Comonad](func: DefFunc[F]): Types[F] =
      func.head.args.foldLeft(
        func.body.map(funcOp[F]).reduceLeft[Types[F]](_ combine _).unsetScope
      ) {
        case (acc, (_, _, ft)) =>
          acc.addIn(Acc.fromType(ft))
      }

    override def block[F[_]: Comonad](block: Block[F]): Types[F] =
      block match {
        case fn: DefFunc[F] =>
          func(fn)
        case deft: DefType[F] =>
          deft.fields.toNel.map {
            case (_, (_, tv)) =>
              Acc.fromType[F](tv)
          }.foldLeft((empty: Types[F]).addOut(Acc.one(deft.name.name.extract, TypeDef(deft))))(_ addIn _)
        case defs: DefService[F] =>
          defs.funcs.toNel.map(_._2).map(Acc.fromType(_)).foldLeft(empty: Types[F])(_ addIn _)
        case a: DefAlias[F] =>
          (empty: Types[F])
            .addOut(Acc.one(a.alias.name.extract, TypeAlias(a.target)))
            .addIn(Acc.fromType(a.target))
      }
  }

  type AbilitiesResolve[F[_]] = InOutAcc[F, Ability[F], AbilityResolve[F]]

  object AbilitiesResolve extends Visitor[AbilitiesResolve] {

    override def funcOp[F[_]: Comonad](op: FuncOp[F]): AbilitiesResolve[F] =
      op match {
        case ar: AbilityResolve[F] =>
          (empty: AbilitiesResolve[F]) addOut Acc.one(ar.ability.name.extract, ar)
        case AbilityFuncCall(ab, _) =>
          (empty: AbilitiesResolve[F]) addIn Acc.one(ab.name.extract, ab)
        case Extract(_, op) =>
          funcOp[F](op)
        case Par(p, op) =>
          funcOp(op).par(p)
        case On(p, ops) =>
          ops
            .widen[FuncOp[F]]
            .map(funcOp[F](_).on(p))
            .reduceLeft(_ combine _)
            .eraseOut
        case _ =>
          empty: AbilitiesResolve[F]
      }

    // Until we have a notion for exporting abilities, they're cleaned
    override def func[F[_]: Comonad](func: DefFunc[F]): AbilitiesResolve[F] =
      func.body.map(funcOp[F]).reduceLeft(_ combine _).unsetScope.eraseOut

    override def block[F[_]: Comonad](block: Block[F]): AbilitiesResolve[F] =
      block match {
        case fn: DefFunc[F] =>
          func(fn)
        case _ => empty: AbilitiesResolve[F]
      }
  }

  type Arrows[F[_]] = InOutAcc[F, ArrowName[F], ArrowMarker[F]]

  object Arrows extends Visitor[Arrows] {

    override def funcOp[F[_]: Comonad](op: FuncOp[F]): Arrows[F] =
      op match {
        case FuncCall(fname, _) =>
          (empty: Arrows[F]) addIn Acc.one(fname.extract, ArrowName(fname))
        case Par(p, op) =>
          funcOp(op).par(p)
        case On(p, ops) =>
          ops
            .widen[FuncOp[F]]
            .map(funcOp[F](_).on(p))
            .reduceLeft(_ combine _)
        case _ =>
          empty: Arrows[F]
      }

    override def func[F[_]: Comonad](func: DefFunc[F]): Arrows[F] =
      func.head.args.foldLeft(
        func.body.map(funcOp[F]).reduceLeft[Arrows[F]](_ combine _).unsetScope
      ) {
        case (acc, (k, _, ft: ArrowType[F])) =>
          acc.subIn(k).addOut(Acc.one(k, LocalArrow(ft)))

        case (acc, _) => acc
      }

    override def block[F[_]: Comonad](block: Block[F]): Arrows[F] =
      block match {
        case fn: DefFunc[F] =>
          func(fn).eraseOut
            .addOut(Acc.one(fn.head.name.name.extract, FuncArrow(fn)))

        case _ =>
          empty: Arrows[F]
      }
  }*/
}
