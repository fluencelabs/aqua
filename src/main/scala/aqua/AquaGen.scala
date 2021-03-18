package aqua

import cats.data.Validated
import cats.effect.{ExitCode, IO, IOApp}

import java.io.{File, PrintWriter}
import scala.io.Source

object AquaGen extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    IO {
      val error = "There should be two arguments: path/to/input/dir and path/to/output/dir"
      val process = for {
        input <- args.headOption.toRight(error)
        output <- args.lift(1).toRight(error)
        inputDir <- {
          val inputDir = new File(input)
          if (!inputDir.isDirectory && !inputDir.exists()) Left("Input path should be a dir and exists")
          else Right(inputDir)
        }
        outputDir <- {
          val outputDir = new File(output)
          if (!outputDir.isDirectory && !outputDir.exists()) Left("Output path should be a dir")
          else Right(outputDir)
        }
        _ = convertAqua(inputDir.listFiles().toList, outputDir)

      } yield {

      }

      process.fold(err => {
        println(err);
        ExitCode.Error
      }, _ => ExitCode.Success)

    }

  def convertAqua(files: List[File], outputDir: File): Unit = {
    for {
      file <- files
    } yield {
      val src = Source.fromFile(file)
      val lines = try src.mkString finally src.close()
      val result = Aqua.generate(lines) match {
        case Validated.Valid(v) ⇒
          v.mkString("\n")
        case Validated.Invalid(errs) ⇒
          errs.map(_.showForConsole(lines)).toList.mkString("\n")
      }
      new PrintWriter(outputDir.toPath.resolve(file.getName + ".result").toFile.getAbsolutePath) { write(result); close() }

    }
  }
}
