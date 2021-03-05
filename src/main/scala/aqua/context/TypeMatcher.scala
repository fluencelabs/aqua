package aqua.context

import aqua.context.marker.{TypeAlias, TypeDef}
import aqua.parser.lexer.{
  ArrayTypeToken,
  ArrowDef,
  ArrowTypeToken,
  BasicTypeToken,
  CustomTypeToken,
  DataTypeToken,
  IntoArray,
  IntoField,
  LambdaOp,
  TypeToken
}
import cats.Comonad
import cats.data.{NonEmptyMap, Validated, ValidatedNel}
import cats.syntax.comonad._

class TypeMatcher[F[_]: Comonad](types: Types[F]) {
  import TypeMatcher._

  // TODO check
  def lambdaDerive(root: DataTypeToken[F], ops: List[LambdaOp[F]]): ValidatedNel[Err, TypeToken[F]] =
    ops match {
      case Nil => Validated.validNel(root)
      case op :: tail =>
        (op, root) match {
          case (IntoArray(_), ArrayTypeToken(data)) => ??? // op must be
          case (IntoField(_), CustomTypeToken(_)) => ??? // get into
        }

//        getType(root).andThen {
//          case Left(fields) =>
//          op match {
//            case IntoField(field) =>
//              fields(field.extract).fold(Validated.invalid("Field not found")){
//                case ft: CustomType[F] => lambdaDerive(ft, tail)
//                case ft if tail.isEmpty =>
//                  Validated.validNel(ft)
//                case _ =>
//                Validated.invalidNel(s"Cannot g")
//              }
//
//          }
//        }
    }

  def getType(t: CustomTypeToken[F]): ValidatedNel[Err, GetType[F]] =
    types.expDef.defineAcc
      .get(t.name.extract)
      .fold[ValidatedNel[Err, GetType[F]]](Validated.invalidNel(s"Undefined type ${t.name.extract}")) {
        case TypeAlias(_, forType: CustomTypeToken[F]) =>
          getType(forType)
        case TypeAlias(_, forType) =>
          Validated.validNel(Right(forType))
        case TypeDef(forDef) =>
          Validated.valid(Left(forDef.fields.map(_._2)))
      }

  // Arrow can take supertypes, return subtypes
  def isArrowSubtype(superArrow: ArrowDef[F], subArrow: ArrowDef[F]): ValidatedNel[Err, Unit] =
    ((superArrow.resType, subArrow.resType) match {
      case (Some(supRes), Some(subRes)) => isSubtype(supRes, subRes)
      case (None, None) => Validated.valid(())
      case _ => Validated.invalidNel("Return types mismatch")
    }) combine (
      if (superArrow.argTypes.length == subArrow.argTypes.length) Validated.validNel(())
      else Validated.invalidNel("Arg length mismatch")
    ) combine (
      subArrow.argTypes
        .zip(superArrow.argTypes)
        .map { case (subT, supT) => isSubtype(subT, supT) }
        .foldLeft(Validated.validNel[Err, Unit](()))(_ combine _)
      )

  def isSubtype(superType: TypeToken[F], subType: TypeToken[F]): ValidatedNel[Err, Unit] =
    (superType, subType) match {
      case (sup: BasicTypeToken[F], sub: BasicTypeToken[F]) if sub.value.extract == sup.value.extract =>
        Validated.Valid(())
      case (sup: BasicTypeToken[F], sub: BasicTypeToken[F]) =>
        // TODO check numbers
        Validated.invalidNel(s"$sup and $sub does not match")
      case (sup: CustomTypeToken[F], sub: CustomTypeToken[F]) if sub.name.extract == sup.name.extract =>
        Validated.validNel(())
      case (sup: CustomTypeToken[F], sub: CustomTypeToken[F]) =>
        getType(sup).product(getType(sub)).andThen {
          case (Left(supFields), Left(subFields)) =>
            // TODO we can collect errors there
            supFields.toNel.map {
              case (fieldName, fieldType) =>
                subFields(fieldName)
                  .map(isSubtype(fieldType, _))
                  .getOrElse(Validated.invalidNel(s"Missing field $fieldName"))
            }.reduceLeft[ValidatedNel[Err, Unit]](_ combine _)

          case (Right(supT), Right(subT)) =>
            isSubtype(supT, subT)

          case _ =>
            // TODO what kind of mismatch?
            Validated.invalidNel("Mismatch!")
        }

      case (sup: ArrowTypeToken[F], sub: ArrowTypeToken[F]) =>
        isArrowSubtype(sup, sub)

      // TODO handle arrays
      // TODO handle defaults
    }
}

object TypeMatcher {
  type Err = String
  type GetType[F[_]] = Either[NonEmptyMap[String, DataTypeToken[F]], TypeToken[F]]

}
