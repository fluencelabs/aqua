package aqua.model.inline.tag

import aqua.errors.Errors.internalError
import aqua.helpers.syntax.reader.*
import aqua.model.*
import aqua.model.ValueModel
import aqua.model.inline.Inline.parDesugarPrefixOpt
import aqua.model.inline.RawValueInliner.valueToModel
import aqua.model.inline.TagInliner.TagInlined
import aqua.model.inline.TagInliner.flat
import aqua.model.inline.state.*
import aqua.raw.ops.{ForKeyValue, ForTag}
import aqua.raw.value.ValueRaw
import aqua.types.{CollectionType, ScalarType, StreamType}

import cats.Eval
import cats.data.Reader
import cats.data.{Chain, State}
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.traverse.*

final case class ForTagInliner(
  item: String,
  iterable: ValueRaw,
  mode: ForTag.Mode,
  keyValue: Option[ForKeyValue]
) {

  def inlined[S: Mangler: Exports: Arrows: Config]: State[S, TagInlined[S]] = for {
    vp <- valueToModel(iterable)
    flattened <- mode match {
      case ForTag.Mode.RecMode => State.pure(vp)
      case _ => flat.tupled(vp)
    }
    (v, p) = flattened
    n <- Mangler[S].findAndForbidName(item)
    elementType = iterable.`type` match {
      case b: CollectionType => b.element
      case _ =>
        internalError(
          s"non-box type variable '$iterable' in 'for' expression."
        )
    }
    itemVar = VarModel(n, elementType)
    _ <- Exports[S].resolved(item, itemVar)
    pref <- keyValue.traverse(kv =>
      for {
        _ <- Exports[S].resolved(kv.key, itemVar.withProperty(IntoFieldModel("key", ScalarType.string)))
        _ <- Exports[S].resolved(kv.value, itemVar.withProperty(IntoFieldModel("value", elementType)))
      } yield EmptyModel
    )
    modeModel = mode match {
      case ForTag.Mode.SeqMode | ForTag.Mode.TryMode => ForModel.Mode.Null
      case ForTag.Mode.ParMode | ForTag.Mode.RecMode => ForModel.Mode.Never
    }
    model = ForModel(n, v, modeModel)
  } yield TagInlined.Around(
    model = StreamRestrictions.restrictStreams(ss => model.wrap(Chain.fromOption(pref) ++ ss)),
    prefix = p
  )
}
