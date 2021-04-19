package aqua.model.func

import aqua.model.VarModel
import aqua.types.Type
import cats.data.Chain

case class ArgsDef(args: List[ArgDef]) {
  def isEmpty: Boolean = args.isEmpty

  def call(c: Call): ArgsCall = ArgsCall(args, c.args)

  def types: List[Type] = args.map(_.`type`)

  def toCallArgs: List[Call.Arg] = args.map(ad => Call.Arg(VarModel(ad.name), ad.`type`))

  lazy val dataArgNames: Chain[String] = Chain.fromSeq(args.collect { case ArgDef.Data(n, _) =>
    n
  })

  lazy val arrowArgs: Chain[ArgDef.Arrow] = Chain.fromSeq(args.collect { case ad: ArgDef.Arrow =>
    ad
  })
}

object ArgsDef {
  val empty: ArgsDef = ArgsDef(Nil)
}
