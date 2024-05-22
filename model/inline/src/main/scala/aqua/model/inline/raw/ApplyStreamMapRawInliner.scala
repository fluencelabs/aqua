package aqua.model.inline.raw

import aqua.model.*
import aqua.model.inline.Inline
import aqua.model.inline.RawValueInliner.unfold
import aqua.model.inline.state.*
import aqua.raw.value.IntoArrowRaw
import aqua.types.{
  ArrayType,
  CanonStreamMapType,
  CanonStreamType,
  DataType,
  ScalarType,
  StreamMapType,
  StreamType,
  StructType
}

import cats.data.NonEmptyMap
import cats.data.{Chain, State}
import cats.syntax.applicative.*
import cats.syntax.monoid.*
import cats.syntax.option.*

object ApplyStreamMapRawInliner {

  private def getIterType(name: String, el: DataType) =
    StructType(name, NonEmptyMap.of("key" -> ScalarType.string, "value" -> ArrayType(el)))

  private def getElementFromMapModel(
    mapName: String,
    mapType: StreamMapType,
    idxVar: ValueModel,
    streamName: String,
    streamCanonName: String,
    iterName: String,
    mapCanonName: String,
    resultName: String
  ): OpModel.Tree = {
    val mapVar = VarModel(mapName, mapType)
    val arrayResultType = ArrayType(mapType.element)
    val streamVar = VarModel(streamName, StreamType(arrayResultType))
    val streamCanonVar = VarModel(streamCanonName, CanonStreamType(arrayResultType))
    val iterableCanon = VarModel(mapCanonName, CanonStreamMapType(arrayResultType))
    val iter = VarModel(iterName, getIterType("iterName_type", mapType.element))
    RestrictionModel(streamName, StreamType(mapType.element)).wrap(
      CanonicalizeModel(mapVar, CallModel.Export(iterableCanon.name, iterableCanon.`type`)).leaf,
      ForModel(iter.name, iterableCanon).wrap(
        XorModel.wrap(
          MatchMismatchModel(
            iterableCanon
              .withProperty(
                IntoFieldModel("key", ScalarType.string)
              ),
            idxVar,
            true
          ).wrap(
            PushToStreamModel(
              iterableCanon
                .withProperty(
                  IntoFieldModel("value", arrayResultType)
                ),
              CallModel.Export(streamVar.name, streamVar.`type`)
            ).leaf
          ),
          NextModel(iter.name).leaf
        )
      ),
      CanonicalizeModel(
        streamVar,
        CallModel.Export(streamCanonVar.name, streamCanonVar.`type`)
      ).leaf,
      FlattenModel(
        streamCanonVar.withProperty(IntoIndexModel("0", arrayResultType)),
        resultName
      ).leaf
    )
  }

  def privateGetElement2(
    mapName: String,
    mapType: StreamMapType,
    idxVar: ValueModel,
    resultName: String,
    mapCanonName: String
  ): OpModel.Tree = {
    val idx = idxVar match {
      case VarModel(name, _, _) =>
        name
      case LiteralModel(literal, _) =>
        literal
    }
    val arrayResultType = ArrayType(mapType.element)
    val mapVar = VarModel(mapName, mapType)
    val canonMap = VarModel(mapCanonName, CanonStreamMapType(mapType.element))
    SeqModel.wrap(
      CanonicalizeModel(mapVar, CallModel.Export(canonMap.name, canonMap.`type`)).leaf,
      FlattenModel(
        canonMap.withProperty(IntoIndexModel(idx, arrayResultType)),
        resultName
      ).leaf
    )
  }

  def apply[S: Mangler: Exports: Arrows: Config](
    mapName: String,
    mapType: StreamMapType,
    idxVar: ValueModel
  ): State[S, (VarModel, Inline)] = {
    // get
    // create stream
    // canon map
    // fold over the canonmap
    // - if key = str
    // -- add elements to stream
    // convert stream to array
    // return this array
    for {
      uniqueResultName <- Mangler[S].findAndForbidName(mapName + "_stream_result")
      uniqueCanonMapName <- Mangler[S].findAndForbidName(mapName + "_canon")
      uniqueCanonStreamName <- Mangler[S].findAndForbidName(mapName + "_canon_stream")
      uniqueStreamName <- Mangler[S].findAndForbidName(mapName + "_stream")
      uniqueIterName <- Mangler[S].findAndForbidName(mapName + "_iter")
    } yield {
      /*val gate = getElementFromMapModel(
        mapName = mapName,
        mapType = mapType,
        idxVar = idxVar,
        streamName = uniqueStreamName,
        streamCanonName = uniqueCanonStreamName,
        iterName = uniqueIterName,
        mapCanonName = uniqueCanonMapName,
        resultName = uniqueResultName
      )*/
      val getResultTree = privateGetElement2(mapName, mapType, idxVar, uniqueResultName, uniqueCanonMapName)

      val inline = Inline(predo = Chain.one(getResultTree))
      val value = VarModel(
        uniqueResultName,
        ArrayType(mapType.element)
      )

      (value, inline)
    }
  }
}
