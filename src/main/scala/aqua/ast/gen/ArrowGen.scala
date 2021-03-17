package aqua.ast.gen

import aqua.ast.algebra.abilities.AbilitiesAlgebra
import aqua.ast.algebra.types.ArrowType
import DataView.InitPeerId
import aqua.parser.lexer.{IntoArray, IntoField, LambdaOp, Literal, Name, Value, VarLambda}
import cats.free.Free

abstract class ArrowGen(val `type`: ArrowType) {

  def gen[F[_], Alg[_]](args: List[Value[F]], result: Option[Name[F]])(implicit
    A: AbilitiesAlgebra[F, Alg]
  ): Free[Alg, AirGen]
}

object ArrowGen {

  private def opsToLens[F[_]](ops: List[LambdaOp[F]]): String =
    ops match {
      case Nil => ""
      case (_: IntoArray[F]) :: tail => "[@" + opsToLens(tail) + "]"
      case (f: IntoField[F]) :: tail => "." + f.value + opsToLens(tail)
    }

  def valueToData[F[_]](v: Value[F]): DataView =
    v match {
      case l: Literal[F] => DataView.StringScalar(l.value)
      case VarLambda(name, Nil) => DataView.Variable(name.value)
      case VarLambda(name, ops) => DataView.VarLens(name.value, opsToLens(ops))
    }

  private def argsToData[F[_]](args: List[Value[F]]): List[DataView] = args.map(valueToData)

  def func(`type`: ArrowType, argNames: List[String], retValue: Option[DataView], bodyGen: FuncBodyGen): ArrowGen =
    new ArrowGen(`type`) {

      override def gen[F[_], Alg[_]](args: List[Value[F]], result: Option[Name[F]])(implicit
        A: AbilitiesAlgebra[F, Alg]
      ): Free[Alg, AirGen] =
        Free.pure[Alg, AirGen](
          bodyGen.op
            .wrap(c =>
              (
                c.copy(data = c.data ++ argNames.zip(argsToData(args))),
                _.copy(data = c.data ++ result.map(_.value).zip(retValue))
              )
            )
        )
    }

  def service(name: String, fnName: String, `type`: ArrowType): ArrowGen =
    new ArrowGen(`type`) {

      override def gen[F[_], Alg[_]](args: List[Value[F]], result: Option[Name[F]])(implicit
        A: AbilitiesAlgebra[F, Alg]
      ): Free[Alg, AirGen] =
        // TODO it's really weird that we're losing token here
        A.getServiceId(name).map {
          case Some(sid) => ServiceCallGen(valueToData(sid), fnName, argsToData(args), result.map(_.value))
          case None =>
            NullGen
        }
    }

  def arg(`type`: ArrowType): ArrowGen =
    new ArrowGen(`type`) {

      override def gen[F[_], Alg[_]](args: List[Value[F]], result: Option[Name[F]])(implicit
        A: AbilitiesAlgebra[F, Alg]
      ): Free[Alg, AirGen] = // TODO resolve generator from context!
        Free.pure[Alg, AirGen](ServiceCallGen(InitPeerId, "arrow name?", Nil, result.map(_.value)))
    }
}
