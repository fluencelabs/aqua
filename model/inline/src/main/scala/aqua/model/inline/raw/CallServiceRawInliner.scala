package aqua.model.inline.raw

import aqua.model.*
import aqua.model.inline.Inline
import aqua.model.inline.RawValueInliner.{callToModel, valueToModel}
import aqua.model.inline.state.{Arrows, Exports, Mangler}
import aqua.raw.ops.Call
import aqua.raw.value.CallServiceRaw
import cats.data.{Chain, State}
import scribe.Logging

object CallServiceRawInliner extends RawInliner[CallServiceRaw] with Logging {

  private[inline] def unfold[S: Mangler: Exports: Arrows](
    value: CallServiceRaw,
    exportTo: List[Call.Export]
  ): State[S, (List[ValueModel], Inline)] = Exports[S].exports.flatMap { exports =>
//    logger.trace(s"${exportTo.mkString(" ")} $value")
//    logger.trace(Console.BLUE + s"call service id ${value.serviceId}" + Console.RESET)


    val call = Call(value.arguments, exportTo)

    for {
      cd <- callToModel(call, true)
      (callModel, callInline) = cd
      sd <- valueToModel(value.serviceId)
      (serviceIdValue, serviceIdInline) = sd
      values = callModel.exportTo.map(e => e.name -> e.asVar.resolveWith(exports)).toMap
      inline = Inline(
        Chain(
          SeqModel.wrap(
            serviceIdInline.toList ++ callInline.toList :+
              CallServiceModel(serviceIdValue, value.fnName, callModel).leaf
          )
        )
      )
      _ <- Exports[S].resolved(values)
      _ <- Mangler[S].forbid(values.keySet)
    } yield values.values.toList -> inline
  }

  override def apply[S: Mangler: Exports: Arrows](
    raw: CallServiceRaw,
    propertiesAllowed: Boolean
  ): State[S, (ValueModel, Inline)] =
    Mangler[S]
      .findAndForbidName(raw.fnName)
      .flatMap { n =>
        unfold(raw, Call.Export(n, raw.`type`) :: Nil).map {
          case (Nil, inline) => (VarModel(n, raw.`type`), inline)
          case (h :: _, inline) => (h, inline)
        }
      }
}
