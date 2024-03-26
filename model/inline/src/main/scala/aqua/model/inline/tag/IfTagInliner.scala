package aqua.model.inline.tag

import aqua.helpers.syntax.reader.*
import aqua.model.*
import aqua.model.ValueModel
import aqua.model.inline.Inline.parDesugarPrefixOpt
import aqua.model.inline.RawValueInliner.valueToModel
import aqua.model.inline.TagInliner.{TagInlined, canonicalizeIfStream}
import aqua.model.inline.state.*
import aqua.raw.value.ApplyBinaryOpRaw.Op as BinOp
import aqua.raw.value.{ApplyBinaryOpRaw, ValueRaw}
import aqua.types.StreamType

import cats.Eval
import cats.data.{Chain, State}
import cats.syntax.apply.*
import cats.syntax.flatMap.*

final case class IfTagInliner(
  valueRaw: ValueRaw
) {
  import IfTagInliner.*

  def inlined[S: Mangler: Exports: Arrows: Config]: State[S, TagInlined[S]] = for {
    cond <- valueRaw match {
      // Optimize in case last operation is equality check
      case ApplyBinaryOpRaw(op @ (BinOp.Eq | BinOp.Neq), left, right, _) =>
        (
          valueToModel(left) >>= canonicalizeIfStream,
          valueToModel(right) >>= canonicalizeIfStream
        ).mapN { case ((lmodel, lprefix), (rmodel, rprefix)) =>
          val prefix = parDesugarPrefixOpt(lprefix, rprefix)
          val shouldMatch = op match {
            case BinOp.Eq => true
            case BinOp.Neq => false
          }

          (prefix, lmodel, rmodel, shouldMatch)
        }
      case _ =>
        valueToModel(valueRaw).map { case (valueModel, prefix) =>
          val compareModel = LiteralModel.bool(true)
          val shouldMatch = true

          (prefix, valueModel, compareModel, shouldMatch)
        }
    }
    (prefix, leftValue, rightValue, shouldMatch) = cond
    noProp <- Config[S].noErrorPropagation.toState
    model = if (noProp) toModelNoProp else toModel
    modelByChildren = model(leftValue, rightValue, shouldMatch)
    stateModel = wrapWithRestrictions[S](modelByChildren)
  } yield TagInlined.Around(
    prefix = prefix,
    model = stateModel
  )

  private def toModelNoProp(
    leftValue: ValueModel,
    rightValue: ValueModel,
    shouldMatch: Boolean
  )(children: Chain[OpModel.Tree]): OpModel.Tree =
    children
      .filterNot(_.head == EmptyModel)
      .uncons
      .map { case (ifBody, elseBody) =>
        XorModel.wrap(
          MatchMismatchModel(
            leftValue,
            rightValue,
            shouldMatch
          ).wrap(ifBody),
          SeqModel.wrap(
            elseBody
          )
        )
      }
      .getOrElse(EmptyModel.leaf)

  private def toModel(
    leftValue: ValueModel,
    rightValue: ValueModel,
    shouldMatch: Boolean
  )(children: Chain[OpModel.Tree]): OpModel.Tree =
    children
      .filterNot(_.head == EmptyModel)
      .uncons
      .map { case (ifBody, elseBody) =>
        val matchFailedErrorCode =
          if (shouldMatch) LiteralModel.matchValuesNotEqualErrorCode
          else LiteralModel.mismatchValuesEqualErrorCode

        /**
         * (xor
         *   ([mis]match left right
         *     <ifBody>
         *   )
         *   (seq
         *     (ap :error: -if-error-)
         *     (xor
         *       (match :error:.$.error_code [MIS]MATCH_FAILED_ERROR_CODE
         *          <falseCase>
         *       )
         *       <errorCase>
         *     )
         *   )
         * )
         */
        def runIf(
          falseCase: Chain[OpModel.Tree],
          errorCase: OpModel.Tree
        ): OpModel.Tree =
          XorModel.wrap(
            MatchMismatchModel(
              leftValue,
              rightValue,
              shouldMatch
            ).wrap(ifBody),
            SeqModel.wrap(
              saveError(ifErrorName).leaf,
              XorModel.wrap(
                MatchMismatchModel(
                  ValueModel.lastErrorCode,
                  matchFailedErrorCode,
                  shouldMatch = true
                ).wrap(falseCase),
                errorCase
              )
            )
          )

        if (elseBody.isEmpty)
          restrictErrors(
            ifErrorName
          )(
            runIf(
              falseCase = Chain.one(NullModel.leaf),
              errorCase = failWithError(ifErrorName).leaf
            )
          )
        else
          restrictErrors(
            ifErrorName,
            elseErrorName,
            ifElseErrorName
          )(
            runIf(
              falseCase = elseBody,
              /**
               * (seq
               *   (ap :error: -else-error-)
               *   (xor
               *     (mismatch :error:.$.error_code [MIS]MATCH_FAILED_ERROR_CODE
               *       (ap -else-error- -if-else-error-)
               *     )
               *     (ap -if-error- -if-else-error)
               *   )
               *   (fail -if-else-error)
               * )
               */
              errorCase = SeqModel.wrap(
                saveError(elseErrorName).leaf,
                XorModel.wrap(
                  MatchMismatchModel(
                    ValueModel.lastErrorCode,
                    LiteralModel.matchValuesNotEqualErrorCode,
                    shouldMatch = true
                  ).wrap(
                    renameError(
                      ifErrorName,
                      ifElseErrorName
                    ).leaf
                  ),
                  renameError(
                    elseErrorName,
                    ifElseErrorName
                  ).leaf
                ),
                failWithError(ifElseErrorName).leaf
              )
            )
          )
      }
      .getOrElse(EmptyModel.leaf)

}

object IfTagInliner {

  private def restrictErrors(
    name: String*
  )(tree: OpModel.Tree): OpModel.Tree =
    name.foldLeft(tree) { case (tree, name) =>
      RestrictionModel(
        name,
        ValueModel.errorType
      ).wrap(tree)
    }

  private def saveError(name: String): FlattenModel =
    FlattenModel(
      ValueModel.error,
      name
    )

  private def renameError(from: String, to: String): FlattenModel =
    FlattenModel(
      VarModel(from, ValueModel.errorType),
      to
    )

  private def failWithError(name: String): FailModel =
    FailModel(
      VarModel(name, ValueModel.errorType)
    )

  private val ifErrorName = "-if-error-"
  private val elseErrorName = "-else-error-"
  private val ifElseErrorName = "-if-else-error-"

  def wrapWithRestrictions[S: Mangler: Exports: Arrows: Config](
    childrenToModel: Chain[OpModel.Tree] => OpModel.Tree
  )(
    children: State[S, Chain[OpModel.Tree]]
  ): State[S, OpModel.Tree] = Exports[S].subScope(for {
    streamsBefore <- Exports[S].streams
    trees <- children
    model = childrenToModel(trees)
    streamsAfter <- Exports[S].streams
    streams = streamsAfter.removedAll(streamsBefore.keySet)
    _ <- Exports[S].deleteStreams(streams.keySet)
  } yield build(model, streams))

  private def build(
    model: OpModel.Tree,
    streams: Map[String, StreamType]
  ): OpModel.Tree =
    streams.toList.foldLeft(model) { case (acc, (name, st)) =>
      RestrictionModel(name, st).wrap(acc)
    }

}
