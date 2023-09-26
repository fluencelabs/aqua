package aqua

import cats.data.{Chain, EitherT, NonEmptyChain, Writer}

package object api {

  type APIWarnings = [A] =>> Writer[
    Chain[String],
    A
  ]

  type APIResult = [A] =>> EitherT[
    APIWarnings,
    NonEmptyChain[String],
    A
  ]
}
