package aqua.model.cursor

import cats.data.{Chain, NonEmptyList}

abstract class ChainCursor[C <: ChainCursor[C, T], T](make: NonEmptyList[ChainZipper[T]] => C) {
  self =>

  val tree: NonEmptyList[ChainZipper[T]]

  def parent: Option[T] = tree.tail.headOption.map(_.current)

  def current: T = tree.head.current

  def leftSiblings: Chain[T] = tree.head.prev
  def rightSiblings: Chain[T] = tree.head.next

  def mapParent(f: T => T): C =
    make(
      NonEmptyList(
        tree.head,
        tree.tail match {
          case h :: t => h.copy(current = f(h.current)) :: t
          case t => t
        }
      )
    )

  def path: NonEmptyList[T] = tree.map(_.current)

  def moveUp: Option[C] = NonEmptyList.fromList(tree.tail).map(make)

  def pathToRoot: LazyList[C] = LazyList.unfold(this)(_.moveUp.map(c => c -> c))

  def moveDown(focusOn: ChainZipper[T]): C = make(focusOn :: tree)

  def moveLeft: Option[C] =
    toPrevSibling orElse moveUp.flatMap(_.moveLeft)

  def moveRight: Option[C] =
    toNextSibling orElse moveUp.flatMap(_.moveRight)

  def toNextSibling: Option[C] = tree.head.moveRight.map(p => make(tree.copy(p)))

  def toPrevSibling: Option[C] = tree.head.moveLeft.map(p => make(tree.copy(p)))

  def allToLeft: LazyList[C] =
    LazyList.unfold(this)(_.moveLeft.map(c => c -> c))

  def allToRight: LazyList[C] =
    LazyList.unfold(this)(_.moveRight.map(c => c -> c))
}
