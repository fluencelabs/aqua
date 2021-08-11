package aqua

import scribe.format._

object LogFormatter {
  val formatter: Formatter = formatter"$date ${string("[")}$levelColored${string("]")} $message$mdc"
}
