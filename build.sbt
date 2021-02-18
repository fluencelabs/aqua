val dottyVersion = "2.13.4"
//val dottyVersion = "3.0.0-M3"

val catsV = "2.3.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "aqua-hll",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    mainClass in (Compile, run) := Some("aqua.Main"),

    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "3.0.0-M5",
      "org.typelevel" %% "cats-parse" % "0.3.0",
      "org.typelevel" %% "cats-free" % catsV
    ),

    libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.2" % Test,
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % Test
  )
