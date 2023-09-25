package aqua

import cats.data.{Chain, EitherT, NonEmptyChain, Writer}

package object compiler {

  type CompileWarnings[S[_]] =
    [A] =>> Writer[Chain[AquaWarning[S]], A]

  type CompileResult[I, E, S[_]] =
    [A] =>> EitherT[CompileWarnings[S], NonEmptyChain[AquaError[I, E, S]], A]
}
