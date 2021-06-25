package aqua.io

import cats.data.EitherT
import cats.effect.kernel.Concurrent
import cats.implicits.toFunctorOps
import fs2.io.file.Files
import fs2.text

import java.nio.file.Path

object FileOps {

  def writeFile[F[_]: Files: Concurrent](file: Path, content: String): EitherT[F, String, Unit] =
    EitherT
      .right[String](Files[F].deleteIfExists(file))
      .flatMap(_ =>
        EitherT[F, String, Unit](
          fs2.Stream
            .emit(
              content
            )
            .through(text.utf8Encode)
            .through(Files[F].writeAll(file))
            .attempt
            .map { e =>
              e.left
                .map(t => s"Error on writing file $file" + t)
            }
            .compile
            .drain
            .map(_ => Right(()))
        )
      )
}
