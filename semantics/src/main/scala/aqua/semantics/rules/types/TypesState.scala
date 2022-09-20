package aqua.semantics.rules.types

import aqua.raw.value.{FunctorRaw, IntoIndexRaw, PropertyRaw, LiteralRaw, ValueRaw}
import aqua.parser.lexer.{ArrayTypeToken, ArrowTypeToken, BasicTypeToken, CustomTypeToken, IntoField, IntoIndex, LambdaOp, Name, OptionTypeToken, StreamTypeToken, Token, TopBottomToken, TypeToken}
import aqua.types.{ArrayType, ArrowType, BottomType, DataType, OptionType, ProductType, StreamType, StructType, TopType, Type}
import cats.data.Validated.{Invalid, Valid}
import cats.data.{Chain, NonEmptyChain, ValidatedNec}
import cats.kernel.Monoid
import aqua.raw.RawContext
import aqua.semantics.lsp.{TokenInfo, TokenType, TokenTypeInfo}

case class TypesState[S[_]](
  fields: Map[String, (Name[S], Type)] = Map.empty[String, (Name[S], Type)],
  strict: Map[String, Type] = Map.empty[String, Type],
  definitions: Map[String, CustomTypeToken[S]] = Map.empty[String, CustomTypeToken[S]],
  fieldsToken: Map[String, TokenTypeInfo[S]] = Map.empty[String, TokenTypeInfo[S]],
  stack: List[TypesState.Frame[S]] = Nil,
  locations: List[(Token[S], TokenInfo[S])] = Nil
) {
  def isDefined(t: String): Boolean = strict.contains(t)

  // TODO: an ugly return type, refactoring
  // Returns type and a token with its definition
  def resolveTypeToken(tt: TypeToken[S]): Option[(Type, List[(Token[S], CustomTypeToken[S])])] =
    tt match {
      case TopBottomToken(_, isTop) =>
        Option((if (isTop) TopType else BottomType, Nil))
      case ArrayTypeToken(_, dtt) =>
        resolveTypeToken(dtt).collect { case (it: DataType, t) =>
          (ArrayType(it), t)
        }
      case StreamTypeToken(_, dtt) =>
        resolveTypeToken(dtt).collect { case (it: DataType, t) =>
          (StreamType(it), t)
        }
      case OptionTypeToken(_, dtt) =>
        resolveTypeToken(dtt).collect { case (it: DataType, t) =>
          (OptionType(it), t)
        }
      case ctt: CustomTypeToken[S] => strict.get(ctt.value).map(t => (t, definitions.get(ctt.value).toList.map(ctt -> _)))
      case btt: BasicTypeToken[S] => Some((btt.value, Nil))
      case ArrowTypeToken(_, args, res) =>
        val strictArgs = args.map(_._2).map(resolveTypeToken).collect { case Some((dt: DataType, t)) =>
          (dt, t)
        }
        val strictRes = res.map(resolveTypeToken).collect { case Some((dt: DataType, t)) =>
          (dt, t)
        }
        Option.when(strictRes.length == res.length && strictArgs.length == args.length){
          val (sArgs, argTokens) = strictArgs.unzip
          val (sRes, resTokens) = strictRes.unzip
          (ArrowType(ProductType(sArgs), ProductType(sRes)), argTokens.flatten ++ resTokens.flatten)
        }
    }

  def resolveArrowDef(ad: ArrowTypeToken[S]): ValidatedNec[(Token[S], String), (ArrowType, List[(Token[S], CustomTypeToken[S])])] = {
    val resType = ad.res.map(resolveTypeToken)

    NonEmptyChain
      .fromChain(Chain.fromSeq(ad.res.zip(resType).collect { case (dt, None) =>
        dt -> "Cannot resolve the result type"
      }))
      .fold[ValidatedNec[(Token[S], String), (ArrowType, List[(Token[S], CustomTypeToken[S])])]] {
        val (errs, argTypes) = ad.args.map { (argName, tt) =>
          resolveTypeToken(tt)
            .toRight(tt -> s"Type unresolved")
            .map(argName.map(_.value) -> _)
        }
          .foldLeft[(Chain[(Token[S], String)], Chain[(Option[String], (Type, List[(Token[S], CustomTypeToken[S])]))])](
            (Chain.empty, Chain.empty[(Option[String], (Type, List[(Token[S], CustomTypeToken[S])]))])
          ) {
            case ((errs, argTypes), Right(at)) => (errs, argTypes.append(at))
            case ((errs, argTypes), Left(e)) => (errs.append(e), argTypes)
          }

        NonEmptyChain
          .fromChain(errs)
          .fold[ValidatedNec[(Token[S], String), (ArrowType, List[(Token[S], CustomTypeToken[S])])]](
            Valid{
              val (labels, types) = argTypes.toList.unzip
              val (resTypes, resTokens) = resType.flatten.unzip
              (ArrowType(
                ProductType.maybeLabelled(labels.zip(types.map(_._1))),
                ProductType(resTypes)
              ), types.map(_._2).flatten ++ resTokens.flatten)
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
