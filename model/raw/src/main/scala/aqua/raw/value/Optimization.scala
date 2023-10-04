package aqua.raw.value

import cats.Eval
import cats.data.Ior
import cats.Semigroup
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.functor.*
import cats.syntax.semigroup.*

object Optimization {

  /**
   * Optimize raw value.
   *
   * A lot more optimizations could be done,
   * it is here just as a proof of concept.
   */
  def optimize(value: ValueRaw): Eval[ValueRaw] =
    Addition.optimize(value)

  object Addition {

    /**
     * Formally speaking it is not a Semigroup
     * as `ValueRaw` is a tree.
     * But it represents addition semigroup logically.
     */
    private given Semigroup[ValueRaw] with {

      def combine(x: ValueRaw, y: ValueRaw): ValueRaw =
        ApplyBinaryOpRaw.Add(x, y)
    }

    def optimize(value: ValueRaw): Eval[ValueRaw] =
      gatherConstantsInAddition(value).map { res =>
        res.leftMap(LiteralRaw.number).merge
      }

    private def gatherConstantsInAddition(
      value: ValueRaw
    ): Eval[Ior[Long, ValueRaw]] =
      value match {
        case ApplyBinaryOpRaw.Add(left, right) =>
          (
            gatherConstantsInAddition(left),
            gatherConstantsInAddition(right)
          ).mapN(_ combine _)
        case LiteralRaw.Integer(i) =>
          Ior.left(i).pure
        case _ =>
          // Optimize expressions inside this value
          Ior.right(value.mapValues(v => optimize(v).value)).pure
      }
  }

}
