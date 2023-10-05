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

    def optimize(value: ValueRaw): Eval[ValueRaw] =
      gatherLiteralsInAddition(value).map { res =>
        res.fold(
          // TODO: Type of literal is not preserved
          LiteralRaw.number,
          identity,
          (literal, nonLiteral) =>
            if (literal > 0) {
              ApplyBinaryOpRaw.Add(nonLiteral, LiteralRaw.number(literal))
            } else if (literal < 0) {
              ApplyBinaryOpRaw.Sub(nonLiteral, LiteralRaw.number(-literal))
            } else nonLiteral
        )
      }

    private def gatherLiteralsInAddition(
      value: ValueRaw
    ): Eval[Ior[Long, ValueRaw]] =
      value match {
        case ApplyBinaryOpRaw.Add(left, right) =>
          (
            gatherLiteralsInAddition(left),
            gatherLiteralsInAddition(right)
          ).mapN(_ add _)
        case ApplyBinaryOpRaw.Sub(
              left,
              /**
               * We support subtraction only with literal at the right side
               * because we don't have unary minus operator for values.
               * (Or this algo should be much more complex to support it)
               * But this case is pretty commonly generated by compiler
               * in gates: `join stream[len - 1]` (see `ApplyGateRawInliner`)
               */
              LiteralRaw.Integer(i)
            ) =>
          (
            gatherLiteralsInAddition(left),
            Eval.now(Ior.left(i))
          ).mapN(_ sub _)
        case LiteralRaw.Integer(i) =>
          Ior.left(i).pure
        case _ =>
          // Optimize expressions inside this value
          Ior.right(value.mapValues(v => optimize(v).value)).pure
      }

    /**
     * Rewritten `Ior.combine` method
     * with custom combination functions.
     */
    private def combineWith(
      l: Ior[Long, ValueRaw],
      r: Ior[Long, ValueRaw]
    )(
      lf: (Long, Long) => Long,
      rf: (ValueRaw, ValueRaw) => ValueRaw
    ): Ior[Long, ValueRaw] =
      l match {
        case Ior.Left(lvl) =>
          r match {
            case Ior.Left(rvl) =>
              Ior.left(lf(lvl, rvl))
            case Ior.Right(rvr) =>
              Ior.both(lvl, rvr)
            case Ior.Both(rvl, rvr) =>
              Ior.both(lf(lvl, rvl), rvr)
          }
        case Ior.Right(lvr) =>
          r match {
            case Ior.Left(rvl) =>
              Ior.both(rvl, lvr)
            case Ior.Right(rvr) =>
              Ior.right(rf(lvr, rvr))
            case Ior.Both(rvl, rvr) =>
              Ior.both(rvl, rf(lvr, rvr))
          }
        case Ior.Both(lvl, lvr) =>
          r match {
            case Ior.Left(rvl) =>
              Ior.both(lf(lvl, rvl), lvr)
            case Ior.Right(rvr) =>
              Ior.both(lvl, rf(lvr, rvr))
            case Ior.Both(rvl, rvr) =>
              Ior.both(lf(lvl, rvl), rf(lvr, rvr))
          }
      }

    extension (l: Ior[Long, ValueRaw]) {

      def add(r: Ior[Long, ValueRaw]): Ior[Long, ValueRaw] =
        combineWith(l, r)(_ + _, ApplyBinaryOpRaw.Add(_, _))

      def sub(r: Ior[Long, ValueRaw]): Ior[Long, ValueRaw] =
        combineWith(l, r)(_ - _, ApplyBinaryOpRaw.Sub(_, _))
    }
  }

}
