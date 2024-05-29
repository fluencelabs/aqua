package aqua.model.inline.raw

import aqua.errors.Errors.internalError
import aqua.model.inline.Inline
import aqua.model.inline.RawValueInliner.unfold
import aqua.model.inline.state.*
import aqua.model.{SeqModel, *}
import aqua.raw.value.IntoArrowRaw
import aqua.types.StreamMapType.Func.*
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

  // make unique name that cannot be used in Aqua
  private def mang[S: Mangler](name: String, suffix: String): State[S, String] =
    Mangler[S].findAndForbidName(s"-${name}_$suffix-")

  private def getStreamModel(
    mapName: String,
    mapType: StreamMapType,
    idxVar: ValueModel,
    streamVar: VarModel,
    iterName: String
  ): OpModel.Tree = {
    val mapVar = VarModel(mapName, mapType)
    val arrayResultType = ArrayType(mapType.element)
    val iter = VarModel(iterName, mapType.iterType("iterName_type"))

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
              CallModel.Export(streamVar)
            ).leaf
          ),
          NextModel(iter.name).leaf
        )
      ),
      NullModel.leaf
    )
  }

  private def getKeysStreamModel(
    mapName: String,
    mapType: StreamMapType,
    streamVar: VarModel,
    iterName: String
  ): OpModel.Tree = {
    val mapVar = VarModel(mapName, mapType)
    val arrayResultType = ArrayType(mapType.element)
    val iter = VarModel(iterName, mapType.iterType("iterName_type"))

    ParModel.wrap(
      ForModel(iter.name, mapVar, ForModel.Mode.Never).wrap(
        PushToStreamModel(
          iter
            .withProperty(
              IntoFieldModel("key", arrayResultType)
            ),
          CallModel.Export(streamVar.name, streamVar.`type`)
        ).leaf,
        NextModel(iter.name).leaf
      ),
      NullModel.leaf
    )
  }

  private def keysModel(
    mapName: String,
    mapType: StreamMapType,
    streamName: String,
    canonName: String,
    iterName: String,
    result: VarModel
  ): OpModel.Tree = {
    val mapVar = VarModel(mapName, mapType)
    val arrayResultType = ArrayType(mapType.element)
    val streamVar = VarModel(streamName, StreamType(ScalarType.string))
    val canonMap = VarModel(canonName, CanonStreamMapType(arrayResultType))
    val iter = VarModel(iterName, mapType.iterType("iterName_type"))

    RestrictionModel(streamVar.name, streamVar.`type`).wrap(
      CanonicalizeModel(mapVar, CallModel.Export(canonMap)).leaf,
      ForModel(iter.name, canonMap).wrap(
        PushToStreamModel(
          iter
            .withProperty(
              IntoFieldModel("key", ScalarType.string)
            ),
          CallModel.Export(streamVar)
        ).leaf,
        NextModel(iter.name).leaf
      ),
      CanonicalizeModel(streamVar, CallModel.Export(result)).leaf
    )

  }

  private def getModel(
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

  private def containsModel(
    mapName: String,
    mapType: StreamMapType,
    keyVar: ValueModel,
    resultArrayName: String,
    resultName: String,
    mapCanonName: String,
    idxName: String
  ): OpModel.Tree = {
    val getElementTree =
      getModel(mapName, mapType, keyVar, resultArrayName, mapCanonName, idxName)
    val arrayResultType = ArrayType(mapType.element)
    val resultArrayVar = VarModel(resultArrayName, arrayResultType)

    SeqModel.wrap(
      getElementTree,
      XorModel.wrap(
        MatchMismatchModel(
          resultArrayVar.withProperty(FunctorModel("length", ScalarType.u32)),
          LiteralModel.number(0),
          true
        ).wrap(
          FlattenModel(LiteralModel.bool(false), resultName).leaf
        ),
        FlattenModel(LiteralModel.bool(true), resultName).leaf
      )
    )
  }

  private def contains[S: Mangler: Exports: Arrows: Config](
    mapName: String,
    mapType: StreamMapType,
    keyVar: ValueModel
  ): State[S, (VarModel, Inline)] = {
    for {
      resultArrayName <- mang(mapName, "result_array")
      resultName <- mang(mapName, "contains_result")
      canonName <- mang(mapName, "canon")
      idxName <- mang(mapName, "idx")
    } yield {
      val result = VarModel(resultName, ScalarType.bool)
      val getKeysTree = containsModel(
        mapName = mapName,
        mapType = mapType,
        keyVar = keyVar,
        resultArrayName = resultArrayName,
        mapCanonName = canonName,
        resultName = resultName,
        idxName = idxName
      )

      val inline = Inline(predo = Chain.one(getKeysTree))

      (result, inline)
    }
  }

  private def getKeys[S: Mangler: Exports: Arrows: Config](
    mapName: String,
    mapType: StreamMapType
  ): State[S, (VarModel, Inline)] = {
    for {
      streamName <- mang(mapName, "stream")
      iterName <- mang(mapName, "iter")
      resultName <- mang(mapName, "keys_result")
      canonName <- mang(mapName, "canon")
      result = VarModel(resultName, CanonStreamType(ScalarType.string))
    } yield {
      val getKeysTree = keysModel(
        mapName = mapName,
        mapType = mapType,
        streamName = streamName,
        iterName = iterName,
        canonName = canonName,
        result = result
      )

      val inline = Inline(predo = Chain.one(getKeysTree))

      (result, inline)
    }
  }

  private def getKeysStream[S: Mangler: Exports: Arrows: Config](
    mapName: String,
    mapType: StreamMapType
  ): State[S, (VarModel, Inline)] = {
    for {
      streamName <- mang(mapName, "keys_stream_result")
      iterName <- mang(mapName, "iter")
      streamVar = VarModel(streamName, StreamType(ArrayType(mapType.element)))
      // add resulted stream to restrict it after inlining
      _ <- Exports[S].resolved(streamName, streamVar)
    } yield {
      val getKeysTree = getKeysStreamModel(
        mapName = mapName,
        mapType = mapType,
        streamVar = streamVar,
        iterName = iterName
      )

      val inline = Inline(predo = Chain.one(getKeysTree))

      (streamVar, inline)
    }
  }

  private def getStream[S: Mangler: Exports: Arrows: Config](
    mapName: String,
    mapType: StreamMapType,
    idxVar: ValueModel
  ): State[S, (VarModel, Inline)] = {
    for {
      streamName <- mang(mapName, "get_stream_result")
      iterName <- mang(mapName, "iter")
      value = VarModel(
        streamName,
        StreamType(mapType.element)
      )
      // add resulted stream to restrict it after inlining
      _ <- Exports[S].resolved(streamName, value)
    } yield {
      val getStreamResultTree = getStreamModel(
        mapName = mapName,
        mapType = mapType,
        idxVar = idxVar,
        streamVar = value,
        iterName = iterName
      )

      val inline = Inline(predo = Chain.one(getStreamResultTree))

      (value, inline)
    }
  }

  private def get[S: Mangler: Exports: Arrows: Config](
    mapName: String,
    mapType: StreamMapType,
    idxVar: ValueModel
  ): State[S, (VarModel, Inline)] = {
    for {
      resultName <- mang(mapName, "get_result")
      canonMapName <- mang(mapName, "canon")
      idxName <- mang(mapName, "idx")
    } yield {
      val getResultTree = getModel(
        mapName,
        mapType,
        idxVar,
        resultName,
        canonMapName,
        idxName
      )

      val inline = Inline(predo = Chain.one(getResultTree))
      val value = VarModel(
        resultName,
        ArrayType(mapType.element)
      )

      (value, inline)
    }
  }

  def apply[S: Mangler: Exports: Arrows: Config](
    funcName: String,
    mapName: String,
    mapType: StreamMapType,
    args: List[ValueModel]
  ): State[S, (VarModel, Inline)] = {
    StreamMapType
      .funcByString(funcName)
      .map(_ -> args)
      .map {
        case (Get, arg :: Nil) =>
          get(mapName, mapType, arg)
        case (GetStream, arg :: Nil) =>
          getStream(mapName, mapType, arg)
        case (Contains, arg :: Nil) =>
          contains(mapName, mapType, arg)
        case (Keys, Nil) =>
          getKeys(mapName, mapType)
        case (KeysStream, Nil) =>
          getKeysStream(mapName, mapType)
        case (n, args) =>
          internalError(
            s"StreamMap '$mapName' has wrong arguments '$args' for function '$n'"
          )
      }
      .getOrElse {
        internalError(
          s"StreamMap '$mapName' doesn't support function '$funcName''"
        )
      }
  }
}
