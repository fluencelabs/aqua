package aqua.model

import aqua.generator.{AirContext, AirGen, DataView, SrvCallable}
import aqua.semantics.Type

case class CoalgebraModel(
  ability: Option[AbilityModel],
  funcName: String,
  args: List[(DataView, Type)],
  exportTo: Option[String]
) extends OpModel {

  def arrowGen: AirGen =
    ability match {
      case Some(ServiceModel(_, id)) =>
        new SrvCallable(id, funcName).toCallGen(args.map(_._1), exportTo)
      case None =>
        (ctx: AirContext) => ctx.arrows(funcName).toCallGen(args.map(_._1), exportTo).generate(ctx)
    }

}
