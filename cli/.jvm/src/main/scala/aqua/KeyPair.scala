package aqua

import scala.concurrent.ExecutionContext
import com.monovore.decline.Command
import cats.effect.ExitCode

object KeyPairOpts {
    def createKeypair[F[_]](implicit ec: ExecutionContext): Command[F[ExitCode]] =
        Command(
            name = "create_keypair",
            header = "Create a new keypair"
        ) { ??? }
}
