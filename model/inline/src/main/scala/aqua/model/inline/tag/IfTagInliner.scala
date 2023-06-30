package aqua.model.inline.tag

import aqua.model.ValueModel
import aqua.model.*

import cats.data.Chain

final case class IfTagInliner(
  left: ValueModel,
  right: ValueModel,
  shouldMatch: Boolean
) {
  import IfTagInliner.*

  def inline(children: Chain[OpModel.Tree]): OpModel.Tree =
    children
      .filterNot(_.head == EmptyModel)
      .uncons
      .map { case (ifBody, elseBody) =>
        val elseOrNull =
          if (elseBody.isEmpty)
            Chain.one(NullModel.leaf)
          else elseBody

        XorModel.wrap(
          MatchMismatchModel(left, right, shouldMatch).wrap(
            ifBody
          ),
          XorModel.wrap(
            noError.wrap(
              elseOrNull
            ),
            rethrow.leaf
          )
        )
      }
      .getOrElse(EmptyModel.leaf)

}

object IfTagInliner {

  val noError: MatchMismatchModel =
    MatchMismatchModel(
      left = ValueModel.lastErrorCode,
      right = LiteralModel.emptyErrorCode,
      shouldMatch = true
    )

  val rethrow: FailModel =
    FailModel(
      value = ValueModel.lastError
    )
}
