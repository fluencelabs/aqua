val dottyVersion = "2.13.4"

lazy val root = project
  .in(file("."))
  .settings(
    name := "aqua-hll",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    mainClass in (Compile, run) := Some("aqua.Main"),

    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "3.0.0-M5",
      "org.typelevel" %% "cats-parse" % "0.3.0"
    ),

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  )
