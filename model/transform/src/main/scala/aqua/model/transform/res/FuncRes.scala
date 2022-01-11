package aqua.model.transform.res

import aqua.model.transform.TransformConfig
import aqua.raw.arrow.Func
import aqua.types.{ArrowType, Type}
import cats.data.Chain
import cats.free.Cofree

// TODO: docs, why source and body here together?
case class FuncRes(
  source: Func,
  conf: TransformConfig,
  body: Cofree[Chain, ResolvedOp]
) {
  import FuncRes.*

  lazy val funcName: String = source.funcName

  lazy val args: List[Arg] = arrowArgs(source.arrowType)
  def argNames: List[String] = source.argNames

  def relayVarName: Option[String] = conf.relayVarName
  def dataServiceId: String = conf.getDataService
  def callbackServiceId: String = conf.callbackService
  def respFuncName: String = conf.respFuncName
  def errorHandlerId: String = conf.errorHandlingService
  def errorFuncName: String = conf.errorFuncName

  // TODO: docs
  def genArgName(basis: String): String = {
    val forbidden = args.map(_._1).toSet
    def genIter(i: Int): String = {
      val n = if (i < 0) basis else basis + i
      if (forbidden(n)) genIter(i + 1) else n
    }
    genIter(-1)
  }

  def returnType: Option[Type] = arrowToRes(source.arrowType)
}

object FuncRes {
  case class Arg(name: String, `type`: Type)
  def arrowArgs(at: ArrowType): List[Arg] = at.domain.toLabelledList().map(Arg(_, _))

  def arrowArgIndices(at: ArrowType): List[Int] =
    LazyList.from(0).take(at.domain.length).toList

  def arrowToRes(at: ArrowType): Option[Type] =
    if (at.codomain.length > 1) Some(at.codomain)
    else at.codomain.uncons.map(_._1)
}
