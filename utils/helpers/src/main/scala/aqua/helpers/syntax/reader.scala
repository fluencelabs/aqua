package aqua.helpers.syntax

import cats.data.{Reader, State}

object reader {

  extension [S, A](r: Reader[S, A]) {
    def toState: State[S, A] = State.inspect(r.run)
  }
}
