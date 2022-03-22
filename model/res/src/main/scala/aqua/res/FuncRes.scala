package aqua.res

import aqua.types.{ArrowType, ProductType, Type}
import cats.data.Chain
import cats.free.Cofree

import scala.annotation.tailrec

// TODO: docs, why source and body here together?
case class FuncRes(
  funcName: String,
  argNames: List[String],
  args: List[FuncRes.Arg],
  returnType: ProductType,
  relayVarName: Option[String],
  dataServiceId: String,
  callbackServiceId: String,
  respFuncName: String,
  errorHandlerId: String,
  errorFuncName: String,
  body: ResolvedOp.Tree
) {

  // TODO: docs
  def genArgName(basis: String): String = {
    val forbidden = args.map(_._1).toSet

    @tailrec
    def genIter(i: Int): String = {
      val n = if (i < 0) basis else basis + i
      if (forbidden(n)) genIter(i + 1) else n
    }

    genIter(-1)
  }
}

object FuncRes {
  case class Arg(name: String, `type`: Type)

  def arrowArgs(at: ArrowType): List[Arg] = at.domain.toLabelledList().map(Arg(_, _))

  def arrowArgIndices(at: ArrowType): List[Int] =
    LazyList.from(0).take(at.domain.length).toList
}
