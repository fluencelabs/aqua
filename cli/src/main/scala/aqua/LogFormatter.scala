package aqua

import scribe.format.*

object LogFormatter {
  val formatter: Formatter = formatter"$date ${string("[")}$levelColored${string("]")} $message$mdc"
  val formatterWithFilename: Formatter = formatter"$date $fileName ${string("[")}$levelColored${string("]")} $message$mdc"
}
