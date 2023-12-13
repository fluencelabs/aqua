package aqua.logging

import scribe.format.*
import scribe.{Level, Logger}

object LogFormatter {

  val formatter: Formatter =
    formatter"$date ${string("[")}$levelColored${string("]")} $messages$mdc"

  val formatterWithFilename: Formatter =
    formatter"$date $fileName ${string("[")}$levelColored${string("]")} $messages$mdc"

  def initLogger(level: Option[Level]): Logger = {
    scribe.Logger.root
      .clearHandlers()
      .clearModifiers()
      .withHandler(formatter = formatter, minimumLevel = level)
      .replace()
  }
}
