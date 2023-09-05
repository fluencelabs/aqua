package aqua.semantics.expr

import aqua.parser.expr.ServiceExpr
import aqua.raw.arrow.{ArrowRaw, FuncRaw}
import aqua.raw.ops.CallArrowRawTag
import aqua.raw.value.{AbilityRaw, CallArrowRaw, ValueRaw, VarRaw}
import aqua.raw.{ConstantRaw, Raw, RawPart, ServiceRaw, TypeRaw}
import aqua.semantics.Prog
import aqua.semantics.rules.ValuesAlgebra
import aqua.semantics.rules.abilities.AbilitiesAlgebra
import aqua.semantics.rules.definitions.DefinitionsAlgebra
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.types.{AbilityType, ArrowType}
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicative.*
import cats.syntax.traverse.*
import cats.Monad
import cats.data.Chain

class ServiceSem[S[_]](val expr: ServiceExpr[S]) extends AnyVal {

  def createAbilityValue[Alg[_]: Monad](id: ValueRaw, abilityType: AbilityType)(implicit
    A: AbilitiesAlgebra[S, Alg],
    N: NamesAlgebra[S, Alg],
    T: TypesAlgebra[S, Alg],
    V: ValuesAlgebra[S, Alg],
    D: DefinitionsAlgebra[S, Alg]
  ) = {}

  // service AbServType("defaultId"):
  // ...


  // AbServType(func1 = ...'defaultId', func2 = ...'defaultId')
  // AbServType.func1()

  // VarRaw(Serv.-default-id-, "defaultId")
  // Serv "new id"
  // VarRaw(Serv.-default-id-, "new id")

  /*
      ability Ab:
        fff: string -> string
        eee: string -> string

      service Serv:
        fff: string -> string

      service Eserv:
        eee: string -> string

      func nested{Ab}:
        ...

      someNewFunc{AbVal}():
        Serv "freferf"
        AbVal.fff()
  
        AbVal.eee()

      func main():
        

        Serv "aaa"
        Serv.fff()

        Eserv "bbb"

        Serv "zzz"
        Serv.fff()

        nested{Serv}()
        AbVal = Ab(fff = Serv.fff, Eserv.eee)
        someNewFunc{AbVal}()
   */



  // ...
  /*
     func main():
       AbServType "some id"
       AbServType(func1 = ...'some id', func2 = ...'some id')

       CallServiceModel('some id')

       AbServType "some id2"
       AbServType(func1 = ...'some id2', func2 = ...'some id2')

       VarRaw(name, arrowType) -> Arrows FuncRaw(name, arrowType)
                                  FuncRaw(name, arrowType, body = ServiceCall)
   */

  private def toFuncRaw(name: String, abilityName: String, serviceId: ValueRaw, t: ArrowType): FuncRaw = {
    val args = t.domain.toLabelledList().map(st => VarRaw(st._1, st._2))
    val rett = t.codomain.toList.headOption
    val ret = rett.map(t => VarRaw("ret", t))
    val raw = CallArrowRaw(None, name, args, t, Some(serviceId))
    val body = ret match {
      case Some(vr) =>
        val callExpr = aqua.raw.ops.Call.Export(vr.name, vr.baseType)
        CallArrowRawTag(callExpr :: Nil, raw)

      case None =>
        CallArrowRawTag(Nil, raw)

    }

    val arrow = ArrowRaw(t, ret.toList, body.leaf)
    val fullName = AbilityType.fullName(abilityName, name)
    FuncRaw(fullName, arrow)
  }

  def program[Alg[_]: Monad](implicit
    A: AbilitiesAlgebra[S, Alg],
    N: NamesAlgebra[S, Alg],
    T: TypesAlgebra[S, Alg],
    V: ValuesAlgebra[S, Alg],
    D: DefinitionsAlgebra[S, Alg]
  ): Prog[Alg, Raw] =
    Prog.after(_ =>
      D.purgeArrows(expr.name).flatMap {
        case Some(nel) =>
          val arrows = nel.map(kv => kv._1.value -> kv._2).toNem
          val abType = AbilityType(expr.name.value, arrows)
          for {
            _ <- T.defineNamedType(expr.name, abType)
            defaultId <- expr.id
              .map(v => V.valueToRaw(v))
              .getOrElse(None.pure[Alg])
            _ <- expr.id.traverse(id => V.ensureIsString(id))

            serviceArrows = defaultId match {
              case Some(did) =>
                val funcs = arrows.mapBoth {
                  case (funcName, t) => funcName -> toFuncRaw(funcName, expr.name.value, did, t)
                }
                funcs.toNel.toList.map(_._2)
              case None => Nil
            }
          } yield {
            println("serviceArrows: " + serviceArrows)
            val tr = TypeRaw(expr.name.value, abType)
            RawPart.Parts(Chain.fromSeq(serviceArrows) :+ tr)
          }
        case None =>
          Raw.error("Service has no arrows, fails").pure[Alg]

      }
    )
}
