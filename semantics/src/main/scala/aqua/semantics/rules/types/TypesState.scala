package aqua.semantics.rules.types

import aqua.raw.value.{FunctorRaw, IntoIndexRaw, LiteralRaw, PropertyRaw, ValueRaw}
import aqua.parser.lexer.*
import aqua.types.*
import aqua.raw.RawContext

import cats.data.Validated.{Invalid, Valid}
import cats.data.{Chain, NonEmptyChain, ValidatedNec}
import cats.kernel.Monoid
import cats.syntax.option.*
import cats.syntax.traverse.*
import cats.syntax.validated.*
import cats.syntax.apply.*
import cats.syntax.bifunctor.*
import cats.syntax.functor.*
import cats.syntax.apply.*

case class TypesState[S[_]](
  fields: Map[String, (Name[S], Type)] = Map(),
  strict: Map[String, Type] = Map.empty,
  definitions: Map[String, NamedTypeToken[S]] = Map(),
  stack: List[TypesState.Frame[S]] = Nil
) {
  def isDefined(t: String): Boolean = strict.contains(t)

  def defineType(name: NamedTypeToken[S], `type`: Type): TypesState[S] =
    copy(
      strict = strict.updated(name.value, `type`),
      definitions = definitions.updated(name.value, name)
    )

  def getType(name: String): Option[Type] =
    strict.get(name)

  def getTypeDefinition(name: String): Option[NamedTypeToken[S]] =
    definitions.get(name)
}

object TypesStateHelper {

  final case class TypeResolution[S[_], +T](
    `type`: T,
    definitions: List[(Token[S], NamedTypeToken[S])]
  )

  final case class TypeResolutionError[S[_]](
    token: Token[S],
    hint: String
  )

  def resolveTypeToken[S[_]](tt: TypeToken[S])(
    state: TypesState[S]
  ): Option[TypeResolution[S, Type]] =
    tt match {
      case TopBottomToken(_, isTop) =>
        val `type` = if (isTop) TopType else BottomType

        TypeResolution(`type`, Nil).some
      case ArrayTypeToken(_, dtt) =>
        resolveTypeToken(dtt)(state).collect { case TypeResolution(it: DataType, t) =>
          TypeResolution(ArrayType(it), t)
        }
      case StreamTypeToken(_, dtt) =>
        resolveTypeToken(dtt)(state).collect { case TypeResolution(it: DataType, t) =>
          TypeResolution(StreamType(it), t)
        }
      case OptionTypeToken(_, dtt) =>
        resolveTypeToken(dtt)(state).collect { case TypeResolution(it: DataType, t) =>
          TypeResolution(OptionType(it), t)
        }
      case ntt: NamedTypeToken[S] =>
        val defs = state
          .getTypeDefinition(ntt.value)
          .toList
          .map(ntt -> _)

        state
          .getType(ntt.value)
          .map(typ => TypeResolution(typ, defs))
      case btt: BasicTypeToken[S] =>
        TypeResolution(btt.value, Nil).some
      case att: ArrowTypeToken[S] =>
        resolveArrowDef(att)(state).toOption
    }

  def resolveArrowDef[S[_]](arrowTypeToken: ArrowTypeToken[S])(
    state: TypesState[S]
  ): ValidatedNec[TypeResolutionError[S], TypeResolution[S, ArrowType]] = {
    val res = arrowTypeToken.res.traverse(typeToken =>
      resolveTypeToken(typeToken)(state)
        .toValidNec(
          TypeResolutionError(
            typeToken,
            "Can not resolve the result type"
          )
        )
    )
    val args = arrowTypeToken.args.traverse { case (argName, typeToken) =>
      resolveTypeToken(typeToken)(state)
        .toValidNec(
          TypeResolutionError(
            typeToken,
            "Can not resolve the argument type"
          )
        )
        .map(argName.map(_.value) -> _)
    }

    (args, res).mapN { (args, res) =>
      val (argsLabeledTypes, argsTokens) =
        args.map { case lbl -> TypeResolution(typ, tkn) =>
          (lbl, typ) -> tkn
        }.unzip.map(_.flatten)
      val (resTypes, resTokens) =
        res.map { case TypeResolution(typ, tkn) =>
          typ -> tkn
        }.unzip.map(_.flatten)

      val typ = ArrowType(
        ProductType.maybeLabelled(argsLabeledTypes),
        ProductType(resTypes)
      )
      val defs = (argsTokens ++ resTokens)

      TypeResolution(typ, defs)
    }
  }
}

object TypesState {

  final case class TypeDefinition[S[_]](
    token: NamedTypeToken[S],
    `type`: Type
  )

  case class Frame[S[_]](
    token: ArrowTypeToken[S],
    arrowType: ArrowType,
    retVals: Option[List[ValueRaw]]
  )

  given [S[_]]: Monoid[TypesState[S]] with {
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
