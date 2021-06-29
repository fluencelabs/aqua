package aqua.compiler.io

import cats.data.EitherT
import cats.effect.Concurrent
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

  def readSourceText[F[_]: Files: Concurrent](
    file: Path
  ): fs2.Stream[F, Either[Throwable, String]] =
    Files[F]
      .readAll(file, 4096)
      .fold(Vector.empty[Byte])((acc, b) => acc :+ b)
      // TODO fix for comment on last line in air
      // TODO should be fixed by parser
      .map(_.appendedAll("\n\r".getBytes))
      .flatMap(fs2.Stream.emits)
      .through(text.utf8Decode)
      .attempt
}
