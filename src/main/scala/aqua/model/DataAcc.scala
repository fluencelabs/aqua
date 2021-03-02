package aqua.model

import aqua.AquaError
import aqua.model.marker.{DataMarker, VarMarker}
import aqua.parser.{AbilityFuncCall, AbilityId, Block, DefFunc, Extract, FuncCall, FuncOp, On, Par}
import aqua.parser.lexer.{DataType, Value, Var}
import cats.Comonad
import cats.data.ValidatedNel
import shapeless._
import shapeless.ops.hlist.Selector
import cats.syntax.comonad._

case class DataAcc[F[_]](acc: InOutAcc[F, Value[F], DataMarker[F, HNil]])

object DataAcc {
  type Acc[F[_]] = InOutAcc[F, Value[F], DataMarker[F, HNil]]
  def emptyAcc[F[_]]: Acc[F] = InOutAcc.empty[F, Value[F], DataMarker[F, HNil]]
  def empty[F[_]]: DataAcc[F] = DataAcc[F](emptyAcc[F])

  class Pass[F[_]: Comonad, I <: HList, O <: HList](extend: Passer[F, I, O])(implicit getScope: Selector[O, Scope[F]])
      extends Passer[F, I, DataAcc[F] :: O] {
    type Ctx = DataAcc[F] :: O

    override def exitFuncOpGroup(group: FuncOp[F, I], last: Ctx): Ctx =
      last.head :: extend.exitFuncOpGroup(group, last.tail)

    override def funcOpCtx(op: FuncOp[F, I], prev: Ctx): Ctx = {
      lazy val in = extend.funcOpCtx(op, prev.tail)
      val data = prev.head
      lazy val mode = getScope(in).mode.map(_.extract)
      def combinedWith(other: Acc[F] => InOutAcc[F, Value[F], DataMarker[F, HNil]]): DataAcc[F] =
        DataAcc[F](data.acc.combine(other(emptyAcc[F]), mode))

      op match {
        case FuncCall(_, args, _) =>
          combinedWith(_ addIn Acc.fromValues(args)) :: in
        case AbilityFuncCall(_, fc, _) =>
          funcOpCtx(fc, prev)
        case Extract(n, fc, _) =>
          val f = funcOpCtx(fc, prev)
          f.head.copy(f.head.acc.combine(empty[F].acc addOut Acc.one(n.name.extract, VarMarker(n)), mode)) :: f.tail
        case AbilityId(_, id, _) =>
          combinedWith(_ addIn Acc.fromValues(id :: Nil)) :: in
        case On(p, _, _) =>
          combinedWith(_ addIn Acc.fromValues(p :: Nil)) :: in
        case Par(_, _, _) =>
          data :: in
      }
    }

    override def blockCtx(block: Block[F, I]): Ctx =
      (block match {
        case DefFunc(head, _, _) =>
          head.args.foldLeft(empty[F]) {
            case (acc, (k, v, _: DataType[F])) =>
              // TODO we know data type there, should we care?
              acc.copy(acc.acc.addOut(Acc.one(k, VarMarker(Var(v)))))
            case (acc, _) => acc
          }

        case _ =>
          empty[F]

      }) :: extend.blockCtx(block)

    override def duplicates(prev: Out, next: Out): List[F[String]] =
      next.head.acc.out
        .takeKeys(prev.head.acc.out.keys)
        .data
        .flatMap {
          case (k, vs) => vs.toList.map(_.toError(s"Duplicated variable definition `$k`"))
        }
        .toList ::: extend.duplicates(prev.tail, next.tail)

    override def emptyCtx: Out = empty[F] :: extend.emptyCtx

    override def combineBlockCtx(prev: Out, block: Out): Out =
      DataAcc(prev.head.acc.eraseOut combineSeq block.head.acc) :: extend.combineBlockCtx(prev.tail, block.tail)

    override def unresolved(ctx: Out): List[F[String]] =
      ctx.head.acc.in.data.flatMap {
        case (k, vs) => vs.toList.map(v => v.as(s"Unresolved variable `$k`"))
      }.toList ::: extend.unresolved(ctx.tail)
  }
}
