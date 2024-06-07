package aqua.semantics.rules.types

import aqua.helpers.data.PName
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
  occurrences: List[(Token[S], PName)]
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

  private def resolveCollection[S[_]](
    tt: TypeToken[S],
    collectionName: String,
    collectionType: DataType => Type
  )(state: TypesState[S]): Res[S, Type] =
    resolveTypeToken(tt)(state).andThen {
      case TypeResolution(it: DataType, t) =>
        TypeResolution(collectionType(it), t).validNec
      case TypeResolution(it, _) =>
        TypeResolutionError(
          tt,
          s"$collectionName could not contain values of type $it"
        ).invalidNec
    }

  def resolveTypeToken[S[_]](
    tt: TypeToken[S]
  )(state: TypesState[S]): Res[S, Type] =
    tt match {
      case TopBottomToken(_, isTop) =>
        val `type` = if (isTop) TopType else BottomType

        TypeResolution(`type`, Nil).validNec
      case ArrayTypeToken(_, dtt) =>
        resolveCollection(dtt, "Array", ArrayType.apply)(state)
      case StreamTypeToken(_, dtt) =>
        resolveCollection(dtt, "Stream", StreamType.apply)(state)
      case StreamMapTypeToken(_, dtt) =>
        resolveCollection(dtt, "StreamMap", StreamMapType.apply)(state)
      case OptionTypeToken(_, dtt) =>
        resolveCollection(dtt, "Option", OptionType.apply)(state)
      case ntt: NamedTypeToken[S] =>
        val defs = (ntt -> ntt.pathName) :: Nil

        state
          .getType(ntt.value)
          .map(typ => TypeResolution(typ, defs))
          .toValidNec(
            TypeResolutionError(
              ntt,
              s"Type ${ntt.value} is not defined"
            )
          )
      case stt: ScalarTypeToken[S] =>
        TypeResolution(stt.value, Nil).validNec
      case att: ArrowTypeToken[S] =>
        resolveArrowDef(att)(state)
    }

  def resolveArrowDef[S[_]](
    arrowTypeToken: ArrowTypeToken[S]
  )(state: TypesState[S]): Res[S, ArrowType] = {
    val res = arrowTypeToken.res
      .traverse(typeToken => resolveTypeToken(typeToken)(state).toEither)
    val args = arrowTypeToken.absWithArgs.traverse { case (argName, typeToken) =>
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
      val defs = argsTokens ++ resTokens

      TypeResolution(typ, defs)
    }.toValidated
  }
}
