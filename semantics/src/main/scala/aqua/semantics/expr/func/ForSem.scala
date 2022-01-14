package aqua.semantics.expr.func

import aqua.raw.Raw
import aqua.parser.expr.func.ForExpr
import aqua.raw.value.ValueRaw
import aqua.raw.ops.*
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.types.{ArrayType, BoxType}
import cats.Monad
import cats.data.Chain
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*

class ForSem[S[_]](val expr: ForExpr[S]) extends AnyVal {

  def program[F[_]: Monad](implicit
    V: ValuesAlgebra[S, F],
    N: NamesAlgebra[S, F],
    T: TypesAlgebra[S, F],
    A: AbilitiesAlgebra[S, F]
  ): Prog[F, Raw] =
    Prog
      .around(
        N.beginScope(expr.item) >> V.valueToRaw(expr.iterable).flatMap[Option[ValueRaw]] {
          case Some(vm) =>
            vm.`type` match {
              case t: BoxType =>
                N.define(expr.item, t.element).as(Option(vm))
              case dt =>
                T.ensureTypeMatches(expr.iterable, ArrayType(dt), dt).as(Option.empty[ValueRaw])
            }

          case _ => None.pure[F]
        },
        (stOpt: Option[ValueRaw], ops: Raw) =>
          N.streamsDefinedWithinScope()
            .map((streams: Set[String]) =>
              (stOpt, ops) match {
                case (Some(vm), op: FuncOp) =>
                  val innerTag = expr.mode.map(_._2).fold[RawTag](SeqTag) {
                    case ForExpr.ParMode => ParTag
                    case ForExpr.TryMode => XorTag
                  }

                  // Restrict the streams created within this scope
                  val body =
                    Chain(
                      streams.toList.foldLeft(op) { case (b, streamName) =>
                        FuncOp.wrap(RestrictionTag(streamName, isStream = true), b)
                      },
                      FuncOp.leaf(NextTag(expr.item.value))
                    )

                  val forTag = FuncOp.wrap(
                    ForTag(expr.item.value, vm),
                    FuncOp.node(
                      expr.mode.map(_._2).fold[RawTag](SeqTag) {
                        case ForExpr.ParMode => ParTag
                        case ForExpr.TryMode => XorTag
                      },
                      body
                    )
                  )
                  // Fix: continue execution after fold par immediately, without finding a path out from par branches
                  if (innerTag == ParTag) FuncOp.wrap(ParTag.Detach, forTag)
                  else forTag
                case _ =>
                  Raw.error("Wrong body of the For expression")
              }
            ) <* N.endScope()
      )
      .abilitiesScope[S](expr.token)
}
