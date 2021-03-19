package aqua.generator

sealed trait ArrowCallable {
  def toCallGen(args: List[DataView], result: Option[String]): AirGen
}

class FuncCallable(argNames: List[String], retValue: Option[DataView], bodyGen: FuncBodyGen) extends ArrowCallable {

  override def toCallGen(args: List[DataView], result: Option[String]): AirGen =
    bodyGen.op
      .wrap(c =>
        (
          c.copy(data = c.data ++ argNames.zip(args)),
          _.copy(data = c.data ++ result.zip(retValue))
        )
      )
}

class SrvCallable(srvId: DataView, fnName: String) extends ArrowCallable {

  override def toCallGen(args: List[DataView], result: Option[String]): AirGen =
    ServiceCallGen(srvId, fnName, args, result)
}

class SrvCallableOnPeer(peerId: DataView, srvId: DataView, fnName: String) extends ArrowCallable {

  override def toCallGen(args: List[DataView], result: Option[String]): AirGen =
    ServiceCallGen(srvId, fnName, args, result).wrap(ctx => (ctx.copy(peerId = peerId), _.copy(peerId = ctx.peerId)))
}
