package aqua.model.inline.raw

import aqua.errors.Errors.internalError
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

  private def getStreamFromMapModel(
    mapName: String,
    mapType: StreamMapType,
    idxVar: ValueModel,
    streamName: String,
    iterName: String
  ): OpModel.Tree = {
    val mapVar = VarModel(mapName, mapType)
    val arrayResultType = ArrayType(mapType.element)
    val streamVar = VarModel(streamName, StreamType(arrayResultType))
    val iter = VarModel(iterName, getIterType("iterName_type", mapType.element))
    ParModel.wrap(
      ForModel(iter.name, mapVar, ForModel.Mode.Never).wrap(
        XorModel.wrap(
          MatchMismatchModel(
            iter
              .withProperty(
                IntoFieldModel("key", ScalarType.string)
              ),
            idxVar,
            true
          ).wrap(
            PushToStreamModel(
              iter
                .withProperty(
                  IntoFieldModel("value", arrayResultType)
                ),
              CallModel.Export(streamVar.name, streamVar.`type`)
            ).leaf
          ),
          NextModel(iter.name).leaf
        )
      ),
      NullModel.leaf
    )
  }

  private def getKeysModel(
    mapName: String,
    mapType: StreamMapType,
    streamName: String,
    canonName: String,
    iterName: String,
    resultName: String
  ): (VarModel, OpModel.Tree) = {
    val mapVar = VarModel(mapName, mapType)
    val arrayResultType = ArrayType(mapType.element)
    val streamVar = VarModel(streamName, StreamType(ScalarType.string))
    val canonMap = VarModel(canonName, CanonStreamMapType(arrayResultType))
    val iter = VarModel(iterName, getIterType("iterName_type", mapType.element))
    val result = VarModel(resultName, CanonStreamType(ScalarType.string))
    result -> RestrictionModel(streamVar.name, streamVar.`type`).wrap(
      CanonicalizeModel(mapVar, CallModel.Export(canonMap)).leaf,
      ForModel(iter.name, canonMap).wrap(
        PushToStreamModel(iter
          .withProperty(
            IntoFieldModel("key", ScalarType.string)
          ), CallModel.Export(streamVar)).leaf,
          NextModel(iter.name).leaf
      ),
      CanonicalizeModel(streamVar, CallModel.Export(result)).leaf,
    )

  }

  def getElement(
    mapName: String,
    mapType: StreamMapType,
    idxVar: ValueModel,
    resultName: String,
    mapCanonName: String,
    idxName: String
  ): OpModel.Tree = {
    val (idx, idxModel) = idxVar match {
      case vm: VarModel =>
        vm -> EmptyModel.leaf
      case lm: LiteralModel =>
        VarModel(idxName, ScalarType.string) -> FlattenModel(lm, idxName).leaf
    }
    val arrayResultType = ArrayType(mapType.element)
    val mapVar = VarModel(mapName, mapType)
    val canonMap = VarModel(mapCanonName, CanonStreamMapType(mapType.element))
    SeqModel.wrap(
      CanonicalizeModel(mapVar, CallModel.Export(canonMap.name, canonMap.`type`)).leaf,
      idxModel,
      FlattenModel(
        canonMap.withProperty(IntoIndexModel(idx.name, arrayResultType)),
        resultName
      ).leaf
    )
  }

  def apply[S: Mangler: Exports: Arrows: Config](
    funcName: String,
    mapName: String,
    mapType: StreamMapType,
    args: List[ValueModel]
  ): State[S, (VarModel, Inline)] = {
    (funcName, args) match {
      case ("get", arg :: Nil) =>
        get(mapName, mapType, arg)
      case ("getStream", arg :: Nil) =>
        getStream(mapName, mapType, arg)
      case ("keys", Nil) =>
        getKeys(mapName, mapType)
      case (n, _) =>
        internalError(
          s"StreamMap '$mapName' doesn't support function '$n''"
        )
    }
  }

  def getKeys[S: Mangler: Exports: Arrows: Config](
    mapName: String,
    mapType: StreamMapType
  ): State[S, (VarModel, Inline)] = {
    for {
      uniqueStreamName <- Mangler[S].findAndForbidName(mapName + "_stream")
      uniqueIterName <- Mangler[S].findAndForbidName(mapName + "_iter")
      uniqueResultName <- Mangler[S].findAndForbidName(mapName + "_result")
      uniqueCanonName <- Mangler[S].findAndForbidName(mapName + "_canon")
    } yield {
      val (value, getKeysTree) = getKeysModel(
        mapName = mapName,
        mapType = mapType,
        streamName = uniqueStreamName,
        iterName = uniqueIterName,
        canonName = uniqueCanonName,
        resultName = uniqueResultName
      )

      val inline = Inline(predo = Chain.one(getKeysTree))

      (value, inline)
    }
  }

  def getStream[S: Mangler: Exports: Arrows: Config](
    mapName: String,
    mapType: StreamMapType,
    idxVar: ValueModel
  ): State[S, (VarModel, Inline)] = {
    for {
      uniqueStreamName <- Mangler[S].findAndForbidName(mapName + "_stream")
      uniqueIterName <- Mangler[S].findAndForbidName(mapName + "_iter")
      value = VarModel(
        uniqueStreamName,
        StreamType(mapType.element)
      )
      _ <- Exports[S].resolved(uniqueStreamName, value)
    } yield {
      val getStreamResultTree = getStreamFromMapModel(
        mapName = mapName,
        mapType = mapType,
        idxVar = idxVar,
        streamName = uniqueStreamName,
        iterName = uniqueIterName
      )

      val inline = Inline(predo = Chain.one(getStreamResultTree))

      (value, inline)
    }
  }

  def get[S: Mangler: Exports: Arrows: Config](
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
      uniqueIdxName <- Mangler[S].findAndForbidName(mapName + "_idx")
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
      val getResultTree = getElement(
        mapName,
        mapType,
        idxVar,
        uniqueResultName,
        uniqueCanonMapName,
        uniqueIdxName
      )

      val inline = Inline(predo = Chain.one(getResultTree))
      val value = VarModel(
        uniqueResultName,
        ArrayType(mapType.element)
      )

      (value, inline)
    }
  }
}
