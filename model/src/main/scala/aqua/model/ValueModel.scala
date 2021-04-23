package aqua.model

import cats.data.Chain

sealed trait ValueModel {
  def resolveWith(map: Map[String, ValueModel]): ValueModel = this
}

case class LiteralModel(value: String) extends ValueModel

object LiteralModel {
  val initPeerId: LiteralModel = LiteralModel("%init_peer_id%")
}

sealed trait LambdaModel
case object IntoArrayModel extends LambdaModel
case class IntoFieldModel(field: String) extends LambdaModel

case class VarModel(name: String, lambda: Chain[LambdaModel] = Chain.empty) extends ValueModel {
  def deriveFrom(vm: VarModel): VarModel = vm.copy(lambda = vm.lambda ++ lambda)

  override def resolveWith(map: Map[String, ValueModel]): ValueModel = {
    map.get(name) match {
      case Some(vv: VarModel) =>
        map.get(vv.name) match {
          case Some(n) =>
            n match {
              /* This case protects from infinite recursion
                 when similar names are in a body of a function and a call of a function
                service Demo("demo"):
                  get4: u64 -> u64

                func two(variable: u64) -> u64:
                    v <- Demo.get4(variable)
                    <- variable

                func three(v: u64) -> u64:
                    variable <- Demo.get4(v)
                    -- here we will try to resolve 'variable' to VarModel('variable')
                    -- that could cause infinite recursion
                    res <- two(variable)
                    <- variable
               */
              case vm @ VarModel(nn, _) if nn == name => deriveFrom(vm)
              // it couldn't go to a cycle as long as the semantics protects it
              case _ => n.resolveWith(map)
            }
          case _ =>
            deriveFrom(vv)
        }

      case Some(vv) => vv // TODO check that lambda is empty, otherwise error
      case None => this // Should not happen
    }
  }
}
