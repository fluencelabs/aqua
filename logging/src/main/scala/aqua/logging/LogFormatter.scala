package aqua.logging

import scribe.format.*
import scribe.{Level, Logger}

object LogFormatter {
  val formatter: Formatter = formatter"$date ${string("[")}$levelColored${string("]")} $message$mdc"

  val formatterWithFilename: Formatter =
    formatter"$date $fileName ${string("[")}$levelColored${string("]")} $message$mdc"

  def initLogger(level: Option[Level]): Logger = {
    scribe.Logger.root
      .clearHandlers()
      .clearModifiers()
      .withHandler(formatter = formatter, minimumLevel = level)
      .replace()
  }
}
