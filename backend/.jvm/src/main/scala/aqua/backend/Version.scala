package aqua.backend

object Version {

  lazy val version = Option(getClass.getPackage.getImplementationVersion)
    .filter(_.nonEmpty)
    .getOrElse("Unknown")
}
