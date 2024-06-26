package aqua.semantics.expr.func

import aqua.helpers.syntax.optiont.*
import aqua.parser.expr.func.DeclareStreamExpr
import aqua.raw.Raw
import aqua.raw.ops.DeclareStreamTag
import aqua.raw.value.VarRaw
import aqua.semantics.Prog
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra

import cats.Monad
import cats.data.OptionT

class DeclareStreamSem[S[_]](val expr: DeclareStreamExpr[S]) {

  def program[Alg[_]: Monad](using
    N: NamesAlgebra[S, Alg],
    T: TypesAlgebra[S, Alg]
  ): Prog[Alg, Raw] = Prog.leaf {
    val sem = for {
      streamType <- OptionT(
        T.resolveStreamType(expr.`type`)
      )
      _ <- OptionT.withFilterF(
        N.define(expr.name, streamType)
      )
    } yield DeclareStreamTag(expr.name.value, streamType).funcOpLeaf: Raw

    sem.getOrElse(Raw.error(s"Name `${expr.name.value}` not defined"))
  }

}
