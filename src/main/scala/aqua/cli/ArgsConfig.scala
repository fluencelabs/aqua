package aqua.cli

import scopt.{OParser, Read}
import scopt.Read.stringRead

import java.io.File
import java.nio.file.Path

case class Config(
  input: Option[List[File]] = None,
  output: Option[Path] = None,
  air: Boolean = false,
  debug: Boolean = false
)

object ArgsConfig {

  implicit val filesListRead: Read[List[File]] =
    stringRead.map { str =>
      val inputDir = new File(str)
      if (!inputDir.isDirectory && !inputDir.exists())
        throw new IllegalArgumentException(s"The input path '$str' must be a directory and exist")
      else
        inputDir.listFiles().toList
    }

  implicit val pathRead: Read[Path] =
    stringRead.map { str =>
      val outputDir = new File(str)
      if (!outputDir.isDirectory && !outputDir.exists())
        throw new IllegalArgumentException(s"The input path '$str' must be a directory and exist")
      else
        outputDir.toPath
    }

  private val builder = OParser.builder[Config]

  private val argParser = {
    import builder._
    OParser.sequence(
      programName("aqua-c"),
      head("aqua-c", "0.1", "Compiles Aquamarine language to TypeScript and AIR"),
      opt[Boolean]('d', "debug")
        .action((x, c) => c.copy(debug = x))
        .text("debug mode (not implemented)"),
      opt[List[File]]('i', "input")
        .action((x, c) => c.copy(input = Some(x)))
        .text("path to directory with aquamarine files"),
      opt[Path]('o', "output")
        .action((x, c) => c.copy(output = Some(x)))
        .text("path to output directory"),
      opt[Unit]('a', "air")
        .action((_, c) => c.copy(air = true))
        .text("generates only a code on air instead of TypeScript file"),
      help('h', "help").text("prints this usage text"),
      checkConfig(c => {
        if (c.input.isEmpty != c.output.isEmpty) {
          failure("'input' and 'output' must be both specified or not specified")
        } else success
      })
    )
  }

  def parseArgs(args: List[String]): Option[Config] = OParser.parse(argParser, args, Config())
}
