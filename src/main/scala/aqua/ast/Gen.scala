package aqua.ast

import cats.Semigroup
import cats.free.Free

import scala.collection.immutable.Queue

sealed trait Gen {
  def lift[F[_]]: Free[F, Gen] = Free.pure(this)
}

object Gen {

  implicit object GenSemigroup extends Semigroup[Gen] {

    override def combine(x: Gen, y: Gen): Gen =
      (x, y) match {
        case (x: ScriptGen, y: ScriptGen) => y.copy(funcs = y.funcs.enqueueAll(x.funcs))
        case (x: FuncGen, y: FuncGen) => ScriptGen(Queue(x, y))
        case (x: FuncGen, y: ScriptGen) => y.copy(funcs = y.funcs.enqueue(x))
        case (x: AirGen, y: FuncBodyGen) => y.copy(op = SeqGen(x, y.op))
        case (x: AirGen, y: ServiceCallGen) => SeqGen(x, y)
        case (x: AirGen, y: ParGen) => ParGen(Some(x), y.right)

        case (NoopGen, _) => y
        case (_, NoopGen) => x

        case (_, y) =>
          println(Console.RED + s"drop x: ${x} in favor of y: $y" + Console.RESET)
          y
      }

  }

  def noop: Gen = NoopGen
  def error: Gen = NoopGen
}

sealed trait AirGen extends Gen {
  def last: AirGen = this
}

case object NoopGen extends Gen

case class SeqGen(left: AirGen, right: AirGen) extends AirGen {
  override def last: AirGen = right
}

case class ServiceCallGen(peerId: String, serviceId: String, fnName: String, args: List[String], result: Option[String])
    extends AirGen

case class FuncBodyGen(op: AirGen) extends Gen

case class FuncGen(name: String, body: FuncBodyGen) extends Gen

case class ScriptGen(funcs: Queue[FuncGen]) extends Gen

case class ParGen(left: Option[AirGen], right: AirGen) extends AirGen {
  override def last: AirGen = right
}
