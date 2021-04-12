package aqua.linker.io

import cats.data.EitherT

import java.io.File
import java.nio.file.Path

case class FileModuleId(value: String)

object FileModuleId {

//  def resolve[F[_]](
//    src: Path,
//    imports: LazyList[Path]
//  ): EitherT[F, LinkerError, (Path, FileModuleId)] =
//    imports.map(f => f.resolve(src)).find(p => p.toFile.isFile)
}
