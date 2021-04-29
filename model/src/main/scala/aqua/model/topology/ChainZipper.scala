package aqua.model.topology

import cats.data.Chain
import cats.free.Cofree

case class ChainZipper[T](prev: Chain[T], current: T, next: Chain[T]) {

  def chain: Chain[T] = (prev :+ current) ++ next

  def moveLeft: Option[ChainZipper[T]] =
    prev.initLast.map { case (init, last) =>
      ChainZipper(init, last, current +: next)
    }

  def moveRight: Option[ChainZipper[T]] =
    next.uncons.map { case (head, tail) =>
      ChainZipper(prev :+ current, head, tail)
    }

  def replaceInjecting(cz: ChainZipper[T]): ChainZipper[T] =
    copy(prev ++ cz.prev, cz.current, cz.next ++ next)
}

object ChainZipper {
  def one[T](el: T): ChainZipper[T] = ChainZipper(Chain.empty, el, Chain.empty)

  def fromChain[T](chain: Chain[T], prev: Chain[T] = Chain.empty): Chain[ChainZipper[T]] =
    chain.uncons.fold(Chain.empty[ChainZipper[T]]) { case (t, next) =>
      ChainZipper(prev, t, next) +: fromChain(next, prev :+ t)
    }

  def fromChainMap[T](chain: Chain[T], prev: Chain[T] = Chain.empty)(
    f: ChainZipper[T] => Option[T]
  ): Chain[T] =
    chain.uncons.fold(Chain.empty[T]) { case (t, next) =>
      f(ChainZipper(prev, t, next)).fold(fromChainMap(next, prev)(f))(r =>
        r +: fromChainMap(next, prev :+ r)(f)
      )
    }

  object Matchers {

    object `current` {
      def unapply[T](cz: ChainZipper[T]): Option[T] = Some(cz.current)
    }

    object `head` {
      def unapply[F[_], T](cz: ChainZipper[Cofree[F, T]]): Option[T] = Some(cz.current.head)
    }
  }
}
