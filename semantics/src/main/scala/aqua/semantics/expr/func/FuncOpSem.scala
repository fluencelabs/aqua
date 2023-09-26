package aqua.semantics.expr.func

import cats.Monad
import cats.syntax.functor.*

import aqua.semantics.rules.names.NamesAlgebra
import aqua.raw.Raw
import aqua.raw.ops.{RawTag, RestrictionTag}

object FuncOpSem {

  def restrictStreamsInScope[S[_], Alg[_]: Monad](tree: RawTag.Tree)(using
    N: NamesAlgebra[S, Alg]
  ): Alg[RawTag.Tree] = N
    .streamsDefinedWithinScope()
    .map(streams =>
      streams.toList
        .foldLeft(tree) { case (tree, (streamName, streamType)) =>
          RestrictionTag(streamName, streamType).wrap(tree)
        }
    )
}
