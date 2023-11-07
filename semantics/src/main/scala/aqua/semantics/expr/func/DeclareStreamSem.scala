package aqua.semantics.expr.func

import aqua.parser.expr.func.DeclareStreamExpr
import aqua.raw.Raw
import aqua.raw.ops.DeclareStreamTag
import aqua.raw.value.VarRaw
import aqua.semantics.Prog
import aqua.semantics.rules.names.NamesAlgebra
import aqua.semantics.rules.types.TypesAlgebra
import aqua.types.*

import cats.Monad
import cats.data.Chain
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*

class DeclareStreamSem[S[_]](val expr: DeclareStreamExpr[S]) {

  def program[Alg[_]: Monad](using
    N: NamesAlgebra[S, Alg],
    T: TypesAlgebra[S, Alg]
  ): Prog[Alg, Raw] = Prog.leaf(
    T.resolveStreamType(expr.`type`).map {
      case Some(streamType) =>
        val valueModel = VarRaw(expr.name.value, streamType)
        DeclareStreamTag(valueModel).funcOpLeaf: Raw
      case None => Raw.error(s"Name `${expr.name.value}` not defined")
    }
  )

}
