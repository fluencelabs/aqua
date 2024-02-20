package aqua.semantics

import cats.Show
import cats.kernel.Order
import cats.syntax.order.*

trait FileId[I] extends Show[I] with Order[I]
