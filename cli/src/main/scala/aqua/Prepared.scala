package aqua

import aqua.AquaCompiler.CompileTarget
import aqua.model.AquaContext
import cats.data.Validated

import java.nio.file.Path

object Prepared {

  /**
   * @param srcFile aqua source
   * @param srcPath a main source path with all aqua files
   * @param targetPath a main path where all output files will be written
   * @param context processed aqua code
   * @return
   */
  def apply(
    srcFile: Path,
    srcPath: Path,
    targetPath: Path,
    context: AquaContext
  ): Validated[Throwable, Prepared] =
    Validated.catchNonFatal {
      val srcDir = if (srcPath.toFile.isDirectory) srcPath else srcPath.getParent
      val srcFilePath = srcDir.toAbsolutePath
        .normalize()
        .relativize(srcFile.toAbsolutePath.normalize())

      val targetDir =
        targetPath.toAbsolutePath
          .normalize()
          .resolve(
            srcFilePath
          )

      new Prepared(targetDir, srcFile, context)
    }
}

/**
 * All info that can be used to write a final output.
 * @param targetDir a directory to write to
 * @param srcFile file with a source (aqua code)
 * @param context processed code
 */
case class Prepared private (targetDir: Path, srcFile: Path, context: AquaContext) {

  def hasOutput(target: CompileTarget): Boolean = target match {
    case _ => context.funcs.nonEmpty
  }

  def targetPath(fileName: String): Validated[Throwable, Path] =
    Validated.catchNonFatal {
      targetDir.getParent.resolve(fileName)
    }
}
