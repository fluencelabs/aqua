package aqua.cli

import scopt.{OParser, Read}
import scopt.Read.stringRead

import java.io.File
import java.nio.file.Path

case class Config(
  input: Option[Path] = None,
  output: Option[Path] = None,
  air: Boolean = false,
  debug: Boolean = false
)

object ArgsConfig {

  implicit val pathRead: Read[Path] =
    stringRead.map { str =>
      val outputDir = new File(str)
      if (!outputDir.isDirectory && !outputDir.exists())
        throw new IllegalArgumentException(s"The path '$str' must be an existing directory")
      else
        outputDir.toPath
    }

  private val builder = OParser.builder[Config]

  private val argParser = {
    import builder._
    OParser.sequence(
      programName("aqua-c"),
      // TODO get version from config
      head("aqua-c", "0.1", "Compiles Aquamarine language to TypeScript and AIR"),
      opt[Boolean]('d', "debug")
        .action((x, c) => c.copy(debug = x))
        .text("debug mode (not implemented)"),
      opt[Path]('i', "input")
        .action((x, c) => c.copy(input = Some(x)))
        .text("path to directory with .aqua files"),
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
