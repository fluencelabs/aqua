package aqua.model

import cats.data.NonEmptyList

case class SeqModel(ops: NonEmptyList[OpModel]) extends OpModel
