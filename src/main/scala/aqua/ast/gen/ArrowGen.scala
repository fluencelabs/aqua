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

  trait Callable {
    def toCallGen(args: List[DataView], result: Option[String]): AirGen
  }

  class FuncCallable(argNames: List[String], retValue: Option[DataView], bodyGen: FuncBodyGen) extends Callable {

    override def toCallGen(args: List[DataView], result: Option[String]): AirGen =
      bodyGen.op
        .wrap(c =>
          (
            c.copy(data = c.data ++ argNames.zip(args)),
            _.copy(data = c.data ++ result.zip(retValue))
          )
        )
  }

  class SrvCallable(srvId: DataView, fnName: String) extends Callable {

    override def toCallGen(args: List[DataView], result: Option[String]): AirGen =
      ServiceCallGen(srvId, fnName, args, result)
  }

  class SrvCallableOnPeer(peerId: DataView, srvId: DataView, fnName: String) extends Callable {

    override def toCallGen(args: List[DataView], result: Option[String]): AirGen =
      ServiceCallGen(srvId, fnName, args, result).wrap(ctx => (ctx.copy(peerId = peerId), _.copy(peerId = ctx.peerId)))
  }

  def func(`type`: ArrowType, argNames: List[String], retValue: Option[DataView], bodyGen: FuncBodyGen): ArrowGen =
    new ArrowGen(`type`) {

      override def gen[F[_], Alg[_]](args: List[Value[F]], result: Option[Name[F]])(implicit
        A: AbilitiesAlgebra[F, Alg]
      ): Free[Alg, AirGen] =
        Free.pure[Alg, AirGen](
          new FuncCallable(argNames, retValue, bodyGen).toCallGen(argsToData(args), result.map(_.value))
        )
    }

  def service(name: String, fnName: String, `type`: ArrowType): ArrowGen =
    new ArrowGen(`type`) {

      override def gen[F[_], Alg[_]](args: List[Value[F]], result: Option[Name[F]])(implicit
        A: AbilitiesAlgebra[F, Alg]
      ): Free[Alg, AirGen] =
        // TODO it's really weird that we're losing token here
        A.getServiceId(name).map {
          case Some(sid) =>
            new SrvCallable(valueToData(sid), fnName).toCallGen(argsToData(args), result.map(_.value))
          case None =>
            NullGen
        }
    }

  def arg(name: String, `type`: ArrowType): ArrowGen =
    new ArrowGen(`type`) {

      override def gen[F[_], Alg[_]](args: List[Value[F]], result: Option[Name[F]])(implicit
        A: AbilitiesAlgebra[F, Alg]
      ): Free[Alg, AirGen] =
        Free.pure[Alg, AirGen](
          new AirGen {

            override def generate(ctx: AirContext): (AirContext, Air) = {
              println(Console.YELLOW + ctx + Console.RESET)
              ctx.arrows(name).toCallGen(argsToData(args), result.map(_.value)).generate(ctx)
            }
          }
        )
    }
}
