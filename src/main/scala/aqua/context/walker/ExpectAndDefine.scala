package aqua.context.walker

import aqua.context.marker.Marker
import aqua.context.scope.{Mode, ParMode, XorMode}
import aqua.parser.lexer.Token
import cats.Comonad
import cats.data.NonEmptyList

case class ExpectAndDefine[F[_], In <: Token[F], Out <: Marker[F]](
  expectAcc: Acc[F, In],
  defineAcc: Acc[F, Out]
) {
  type Self = ExpectAndDefine[F, In, Out]

  def combine(other: Self, mode: Option[Mode])(implicit F: Comonad[F]): Self =
    mode match {
      case None => combineSeq(other)
      case Some(XorMode) => combineXor(other)
      case Some(ParMode) => combinePar(other)
    }

  def combineSeq(other: Self): Self =
    copy(expectAcc = expectAcc.add(other.expectAcc, defineAcc.keys), defineAcc = defineAcc add other.defineAcc)

  def combinePar(other: Self): Self =
    copy(expectAcc = expectAcc add other.expectAcc, defineAcc = defineAcc add other.defineAcc)

  def combineXor(other: Self): Self =
    copy(expectAcc = expectAcc add other.expectAcc)

  def expect(addition: Acc[F, In]): Self =
    copy(expectAcc = expectAcc add addition)

  def resolved(rem: String): Self =
    copy(expectAcc = expectAcc sub rem)

  def defined(addition: Acc[F, Out]): Self =
    copy(defineAcc = defineAcc add addition)

  def collectDefinitions(pf: PartialFunction[Out, Out]): Self =
    copy(defineAcc = defineAcc.copy(data = defineAcc.data.map {
      case (k, v) => k -> v.toList.collect(pf)
    }.collect {
      case (k, h :: tail) => k -> NonEmptyList[Out](h, tail)
    }))

  def undefine(rem: String): Self =
    copy(defineAcc = defineAcc sub rem)

  def clearDefinitions: Self = copy(defineAcc = defineAcc.erase)
  def clearExpectations: Self = copy(expectAcc = expectAcc.erase)
}

object ExpectAndDefine {

  def empty[F[_], In <: Token[F], Out <: Marker[F]]: ExpectAndDefine[F, In, Out] =
    ExpectAndDefine(Acc.empty[F, In], Acc.empty[F, Out])

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
