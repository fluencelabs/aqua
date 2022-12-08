package aqua.backend.api

import aqua.backend.air.AirBackend
import aqua.backend.{Backend, Generated}
import aqua.res.AquaRes
import aqua.definition.{LabeledProductTypeDef, ArrowTypeDef, ServiceDef}

object APIBackend extends Backend {

  override def generate(res: AquaRes): Seq[Generated] =
    println("GENERATING IN BACKEND")
    println("funcs: " + res.funcs)
    println("services: " + res.services)
    if (res.isEmpty) Nil
    else {
      val airGenerated = AirBackend.generate(res)

      val services = res.services.map { srv =>
          val functions = LabeledProductTypeDef(
            srv.members.map { case (n, a) => (n, ArrowTypeDef(a)) }
          )

          ServiceDef(srv.defaultId.map(s => s.replace("\"", "")), functions)
      }.toList

      println("air: " + airGenerated)

      Generated("", "", airGenerated.flatMap(_.air).toList, services) :: Nil
    }
}
