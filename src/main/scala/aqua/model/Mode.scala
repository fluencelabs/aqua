package aqua.model

sealed trait Mode
case object ParMode extends Mode
case object XorMode extends Mode
