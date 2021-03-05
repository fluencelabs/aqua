package aqua.interim

case class Op(label: String, ops: List[Ctx => Op])

case class Ctx(on: String, prefix: String)

case class Func(args: List[(String, Type)], body: Ctx => Op)
