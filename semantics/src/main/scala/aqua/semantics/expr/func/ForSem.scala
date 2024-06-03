package aqua.semantics.expr.func

import aqua.parser.expr.func.ForExpr
import aqua.parser.lexer.{Name, ValueToken}
import aqua.raw.Raw
import aqua.raw.ops.*
import aqua.raw.ops.ForTag.Mode
import aqua.raw.value.ValueRaw
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.mangler.ManglerAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.types.*

import cats.Monad
import cats.data.{Chain, OptionT}
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.option.*

class ForSem[S[_]](val expr: ForExpr[S]) extends AnyVal {

  def program[F[_]: Monad](using
    V: ValuesAlgebra[S, F],
    N: NamesAlgebra[S, F],
    A: AbilitiesAlgebra[S, F],
    M: ManglerAlgebra[F]
  ): Prog[F, Raw] =
    Prog
      .around(
        ForSem.beforeFor(expr.item, expr.iterable),
        // Without type of ops specified
        // scala compiler fails to compile this
        (iterable, ops: Raw) =>
          (iterable, ops) match {
            case (Some(vm), FuncOp(op)) =>
              val mode = expr.mode.fold(ForTag.Mode.SeqMode) {
                case ForExpr.Mode.ParMode => ForTag.Mode.ParMode
                case ForExpr.Mode.TryMode => ForTag.Mode.TryMode
                case ForExpr.Mode.RecMode => ForTag.Mode.RecMode
              }

              val innerTag = mode match {
                case ForTag.Mode.SeqMode => SeqTag
                case ForTag.Mode.ParMode => ParTag
                case ForTag.Mode.TryMode => TryTag
                case ForTag.Mode.RecMode => ParTag
              }

              val (item, pair) = ForSem.itemOrPair(expr.item)

              val forTag = ForTag(item, vm, mode, pair).wrap(
                innerTag.wrap(
                  op,
                  NextTag(item).leaf
                )
              )

              // Fix: continue execution after fold par immediately,
              // without finding a path out from par branches
              val result = mode match {
                case ForTag.Mode.ParMode | ForTag.Mode.RecMode =>
                  ParTag.Detach.wrap(forTag)
                case _ => forTag
              }

              result.toFuncOp.pure
            case _ => Raw.error("Wrong body of the `for` expression").pure[F]
          }
      )
      .namesScope(expr.token)
      .abilitiesScope(expr.token)
}

object ForSem {

  def itemOrPair[S[_]](nameOrPair: ForExpr.NameOrPair[S]): (String, Option[ForKeyValue]) =
    nameOrPair match {
      case Right(v) => (v.value, None)
      case Left(k, v) => ("-iterable-", ForKeyValue(k.value, v.value).some)
    }

  def beforeFor[S[_], F[_]: Monad](
    item: ForExpr.NameOrPair[S],
    iterable: ValueToken[S]
  )(using
    V: ValuesAlgebra[S, F],
    N: NamesAlgebra[S, F],
    M: ManglerAlgebra[F]
  ): F[Option[ValueRaw]] = (for {
    value <- V.valueToIterable(iterable)
    (raw, typ) = value
    res <- (typ, item) match {
      case (smt: StreamMapType, Left(key, value)) =>
        OptionT.liftF(for {
          _ <- N.define(key, ScalarType.string)
          _ <- N.define(value, smt.element)
        } yield raw)
      case (smt: StreamMapType, Right(it)) =>
        val typeName = s"KVPair(${smt.element})"
        OptionT.liftF(for {
          newTypeName <- M.rename(typeName)
          iterType = smt.iterType(newTypeName)
          _ <- N.define(it, iterType)
        } yield raw)
      case (_, Left(_, _)) =>
        OptionT.none
      case (_, Right(it)) =>
        OptionT.liftF(N.define(it, typ.element).map(_ => raw))
    }
  } yield res).value
}
