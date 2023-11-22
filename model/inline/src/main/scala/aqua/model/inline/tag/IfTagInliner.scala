package aqua.model.inline.tag

import aqua.raw.value.{ApplyBinaryOpRaw, ValueRaw}
import aqua.raw.value.ApplyBinaryOpRaw.Op as BinOp
import aqua.model.ValueModel
import aqua.model.*
import aqua.model.inline.state.{Arrows, Exports, Mangler}
import aqua.model.inline.RawValueInliner.valueToModel
import aqua.model.inline.TagInliner.canonicalizeIfStream
import aqua.model.inline.Inline.parDesugarPrefixOpt

import cats.data.Chain
import cats.syntax.flatMap.*
import cats.syntax.apply.*

final case class IfTagInliner(
  valueRaw: ValueRaw
) {
  import IfTagInliner.*

  def inlined[S: Mangler: Exports: Arrows] =
    (valueRaw match {
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
    }).map { case (prefix, leftValue, rightValue, shouldMatch) =>
      IfTagInlined(
        prefix,
        toModel(leftValue, rightValue, shouldMatch)
      )
    }

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

  final case class IfTagInlined(
    prefix: Option[OpModel.Tree],
    toModel: Chain[OpModel.Tree] => OpModel.Tree
  )

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

}
