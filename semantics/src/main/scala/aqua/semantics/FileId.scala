package aqua.semantics

import cats.Show
import cats.kernel.Order

trait FileId[I] extends Show[I] with Order[I]
