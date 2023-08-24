package aqua.semantics.rules.types

import aqua.raw.value.{FunctorRaw, IntoIndexRaw, LiteralRaw, PropertyRaw, ValueRaw}
import aqua.parser.lexer.*
import aqua.types.*
import aqua.raw.RawContext

import cats.data.Validated.{Invalid, Valid}
import cats.data.{Chain, NonEmptyChain, ValidatedNec}
import cats.kernel.Monoid
import cats.syntax.option.*

case class TypesState[S[_]](
  fields: Map[String, (Name[S], Type)] = Map.empty[String, (Name[S], Type)],
  strict: Map[String, Type] = Map.empty[String, Type],
  definitions: Map[String, NamedTypeToken[S]] = Map.empty[String, NamedTypeToken[S]],
  stack: List[TypesState.Frame[S]] = Nil
) {
  def isDefined(t: String): Boolean = strict.contains(t)
}

object TypesStateHelper {

  // TODO: an ugly return type, refactoring
  // Returns type and a token with its definition
  def resolveTypeToken[S[_]](
    tt: TypeToken[S],
    state: TypesState[S],
    resolver: (
      TypesState[S],
      NamedTypeToken[S]
    ) => Option[(Type, List[(Token[S], NamedTypeToken[S])])]
  ): Option[(Type, List[(Token[S], NamedTypeToken[S])])] =
    tt match {
      case TopBottomToken(_, isTop) =>
        (if (isTop) TopType else BottomType, Nil).some
      case ArrayTypeToken(_, dtt) =>
        resolveTypeToken(dtt, state, resolver).collect { case (it: DataType, t) =>
          (ArrayType(it), t)
        }
      case StreamTypeToken(_, dtt) =>
        resolveTypeToken(dtt, state, resolver).collect { case (it: DataType, t) =>
          (StreamType(it), t)
        }
      case OptionTypeToken(_, dtt) =>
        resolveTypeToken(dtt, state, resolver).collect { case (it: DataType, t) =>
          (OptionType(it), t)
        }
      case ctt: NamedTypeToken[S] =>
        resolver(state, ctt)
      case btt: BasicTypeToken[S] => (btt.value, Nil).some
      case ArrowTypeToken(_, args, res) =>
        val strictArgs =
          args.map(_._2).map(resolveTypeToken(_, state, resolver)).collect {
            case Some((dt: DataType, t)) =>
              (dt, t)
          }
        val strictRes =
          res.map(resolveTypeToken(_, state, resolver)).collect { case Some((dt: DataType, t)) =>
            (dt, t)
          }
        Option.when(strictRes.length == res.length && strictArgs.length == args.length) {
          val (sArgs, argTokens) = strictArgs.unzip
          val (sRes, resTokens) = strictRes.unzip
          (ArrowType(ProductType(sArgs), ProductType(sRes)), argTokens.flatten ++ resTokens.flatten)
        }
    }

  def resolveArrowDef[S[_]](
    arrowTypeToken: ArrowTypeToken[S],
    state: TypesState[S],
    resolver: (
      TypesState[S],
      NamedTypeToken[S]
    ) => Option[(Type, List[(Token[S], NamedTypeToken[S])])]
  ): ValidatedNec[(Token[S], String), (ArrowType, List[(Token[S], NamedTypeToken[S])])] = {
    val resType = arrowTypeToken.res.map(resolveTypeToken(_, state, resolver))

    NonEmptyChain
      .fromChain(Chain.fromSeq(arrowTypeToken.res.zip(resType).collect { case (dt, None) =>
        dt -> "Cannot resolve the result type"
      }))
      .fold[ValidatedNec[(Token[S], String), (ArrowType, List[(Token[S], NamedTypeToken[S])])]] {
        val (errs, argTypes) = arrowTypeToken.args.map { (argName, tt) =>
          resolveTypeToken(tt, state, resolver)
            .toRight(tt -> s"Type unresolved")
            .map(argName.map(_.value) -> _)
        }
          .foldLeft[
            (
              Chain[(Token[S], String)],
              Chain[(Option[String], (Type, List[(Token[S], NamedTypeToken[S])]))]
            )
          ](
            (
              Chain.empty,
              Chain.empty[(Option[String], (Type, List[(Token[S], NamedTypeToken[S])]))]
            )
          ) {
            case ((errs, argTypes), Right(at)) => (errs, argTypes.append(at))
            case ((errs, argTypes), Left(e)) => (errs.append(e), argTypes)
          }

        NonEmptyChain
          .fromChain(errs)
          .fold[ValidatedNec[
            (Token[S], String),
            (ArrowType, List[(Token[S], NamedTypeToken[S])])
          ]](
            Valid {
              val (labels, types) = argTypes.toList.unzip
              val (resTypes, resTokens) = resType.flatten.unzip
              (
                ArrowType(
                  ProductType.maybeLabelled(labels.zip(types.map(_._1))),
                  ProductType(resTypes)
                ),
                types.flatMap(_._2) ++ resTokens.flatten
              )
            }
          )(Invalid(_))
      }(Invalid(_))
  }
}

object TypesState {

  case class Frame[S[_]](
    token: ArrowTypeToken[S],
    arrowType: ArrowType,
    retVals: Option[List[ValueRaw]]
  )

  implicit def typesStateMonoid[S[_]]: Monoid[TypesState[S]] = new Monoid[TypesState[S]] {
    override def empty: TypesState[S] = TypesState()

    override def combine(x: TypesState[S], y: TypesState[S]): TypesState[S] =
      TypesState(
        strict = x.strict ++ y.strict,
        definitions = x.definitions ++ y.definitions
      )
  }

  def init[S[_]](context: RawContext): TypesState[S] =
    TypesState(strict = context.allTypes)
}
