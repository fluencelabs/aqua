package aqua.semantics.rules.types

import aqua.parser.lexer.*
import aqua.types.*

import cats.data.ValidatedNec
import cats.syntax.apply.*
import cats.syntax.bifunctor.*
import cats.syntax.either.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import cats.syntax.validated.*

final case class TypeResolution[S[_], +T](
  `type`: T,
  definitions: List[(Token[S], NamedTypeToken[S])]
)

object TypeResolution {

  final case class TypeResolutionError[S[_]](
    token: Token[S],
    hint: String
  )

  type Res[S[_], A] = ValidatedNec[
    TypeResolutionError[S],
    TypeResolution[S, A]
  ]

  def resolveTypeToken[S[_]](
    tt: TypeToken[S]
  )(state: TypesState[S]): Res[S, Type] =
    tt match {
      case TopBottomToken(_, isTop) =>
        val `type` = if (isTop) TopType else BottomType

        TypeResolution(`type`, Nil).validNec
      case ArrayTypeToken(_, dtt) =>
        resolveTypeToken(dtt)(state).andThen {
          case TypeResolution(it: DataType, t) =>
            TypeResolution(ArrayType(it), t).validNec
          case TypeResolution(it, _) =>
            TypeResolutionError(
              dtt,
              s"Array could not contain values of type $it"
            ).invalidNec
        }
      case StreamTypeToken(_, dtt) =>
        resolveTypeToken(dtt)(state).andThen {
          case TypeResolution(it: DataType, t) =>
            TypeResolution(StreamType(it), t).validNec
          case TypeResolution(it, _) =>
            TypeResolutionError(
              dtt,
              s"Stream could not contain values of type $it"
            ).invalidNec
        }
      case OptionTypeToken(_, dtt) =>
        resolveTypeToken(dtt)(state).andThen {
          case TypeResolution(it: DataType, t) =>
            TypeResolution(OptionType(it), t).validNec
          case TypeResolution(it, _) =>
            TypeResolutionError(
              dtt,
              s"Option could not contain values of type $it"
            ).invalidNec
        }
      case ntt: NamedTypeToken[S] =>
        val defs = state
          .getTypeDefinition(ntt.value)
          .toList
          .map(ntt -> _)

        state
          .getType(ntt.value)
          .map(typ => TypeResolution(typ, defs))
          .toValidNec(
            TypeResolutionError(
              ntt,
              s"Type ${ntt.value} is not defined"
            )
          )
      case btt: BasicTypeToken[S] =>
        TypeResolution(btt.value, Nil).validNec
      case att: ArrowTypeToken[S] =>
        resolveArrowDef(att)(state)
    }

  def resolveArrowDef[S[_]](
    arrowTypeToken: ArrowTypeToken[S]
  )(state: TypesState[S]): Res[S, ArrowType] = {
    val res = arrowTypeToken.res
      .traverse(typeToken => resolveTypeToken(typeToken)(state).toEither)
    val args = arrowTypeToken.args.traverse { case (argName, typeToken) =>
      resolveTypeToken(typeToken)(state)
        .map(argName.map(_.value) -> _)
        .toEither
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
    }.toValidated
  }
}
