package aqua.semantics.expr.func

import aqua.raw.ops.{RawTag, RestrictionTag}
import aqua.semantics.rules.names.NamesAlgebra
import cats.{FlatMap, Functor, Monad}
import cats.syntax.functor.*
import cats.syntax.flatMap.*

object FuncOpSem {

  def restrictStreamsInScope[S[_], Alg[_]: Functor: FlatMap](
    tree: RawTag.Tree
  )(using N: NamesAlgebra[S, Alg]): Alg[RawTag.Tree] = for {
    allNames <- N.getAllNames()
    d <- N
      .streamsDefinedWithinScope()
      .map { streams =>
        streams.toList
          .foldLeft(tree) { case (tree, (streamName, streamType)) =>
            RestrictionTag(streamName, streamType).wrap(tree)
          }
      }
  } yield d
}
