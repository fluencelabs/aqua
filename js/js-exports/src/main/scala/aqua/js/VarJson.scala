package aqua.js

import aqua.raw.value.{ValueRaw, VarRaw}
import cats.data.Validated.{invalidNec, validNec}
import cats.data.ValidatedNec
import cats.syntax.traverse.*

import scala.scalajs.js

// Variable and its JSON value
case class VarJson(variable: VarRaw, value: js.Dynamic)

object VarJson {

  // checks if data is presented if there is non-literals in function arguments
  // creates services to add this data into a call
  def checkDataGetServices(
    args: List[ValueRaw] = Nil,
    data: Option[js.Dynamic]
  ): ValidatedNec[String, (List[ValueRaw], Map[String, VarJson])] = {
    val vars = args.collect { case v @ VarRaw(_, _) =>
      v
    // one variable could be used multiple times
    }.distinctBy(_.name)

    data match {
      case None if vars.nonEmpty =>
        // TODO: add a list  with actual argument names that where present in the function call
        invalidNec("Missing variables. You can provide them via --data or --data-path flags")
      case None =>
        validNec((args, Map.empty))
      case Some(data) =>
        vars.map { vm =>
          val arg = {
            val a = data.selectDynamic(vm.name)
            if (js.isUndefined(a)) null
            else a
          }

          val typeV = JsonEncoder.aquaTypeFromJson(vm.name, arg)

          typeV.map(t => (vm.copy(baseType = t), arg))
        }.sequence
          .map(_.map { case (vm, arg) =>
            vm.name -> VarJson(vm, arg)
          }.toMap)
          .andThen { services =>
            val argsWithTypes = args.map {
              case v @ VarRaw(n, _) =>
                // argument getters have been enriched with types derived from JSON
                // put this types to unriched arguments in CliFunc
                services.get(n).map(g => v.copy(baseType = g._1.baseType)).getOrElse(v)
              case v => v
            }

            validNec((argsWithTypes, services))
          }
    }
  }
}
