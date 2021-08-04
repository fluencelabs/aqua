package aqua.model.func

import aqua.model.VarModel
import aqua.types.Type
import cats.data.Chain

// TODO replace with ProductType
case class ArgsDef(args: List[ArgDef]) {
  def isEmpty: Boolean = args.isEmpty

  def types: List[Type] = args.map(_.`type`)

  def toCallArgs: List[VarModel] = args.map(ad => VarModel(ad.name, ad.`type`))

  lazy val dataArgs: Chain[ArgDef.Data] = Chain.fromSeq(args.collect { case ad: ArgDef.Data =>
    ad
  })

  lazy val arrowArgs: Chain[ArgDef.Arrow] = Chain.fromSeq(args.collect { case ad: ArgDef.Arrow =>
    ad
  })
}

object ArgsDef {
  val empty: ArgsDef = ArgsDef(Nil)
}
