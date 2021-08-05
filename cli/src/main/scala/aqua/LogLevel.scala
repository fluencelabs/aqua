package aqua

import wvlet.log.{LogLevel => WLogLevel}

sealed trait LogLevel extends EnumEntry with EnumEntry.Lowercase

object LogLevel extends Enum[LogLevel] {
  case object Debug extends LogLevel
  case object Trace extends LogLevel
  case object Info extends LogLevel
  case object Off extends LogLevel
  case object Warn extends LogLevel
  case object Error extends LogLevel
  case object All extends LogLevel

  val values = findValues

  def toLogLevel(logLevel: LogLevel): WLogLevel = {
    logLevel match {
      case LogLevel.Debug => WLogLevel.DEBUG
      case LogLevel.Trace => WLogLevel.TRACE
      case LogLevel.Info => WLogLevel.INFO
      case LogLevel.Off => WLogLevel.OFF
      case LogLevel.Warn => WLogLevel.WARN
      case LogLevel.Error => WLogLevel.ERROR
      case LogLevel.All => WLogLevel.ALL
    }
  }
}
