val dottyVersion = "2.13.5"
//val dottyVersion = "3.0.0-RC1"

val catsV = "2.4.2"
val monocleV = "3.0.0-M3"

lazy val root = project
  .in(file("."))
  .settings(
    name                        := "aqua-hll",
    version                     := "0.1.0",
    scalaVersion                := dottyVersion,
    mainClass in (Compile, run) := Some("aqua.AquaGen"),
    mainClass in assembly       := Some("aqua.AquaGen"),
    libraryDependencies ++= Seq(
      "org.typelevel"              %% "cats-effect"   % "3.0.0-RC2",
      "org.typelevel"              %% "cats-parse"    % "0.3.1",
      "org.typelevel"              %% "cats-free"     % catsV,
      "com.github.julien-truffaut" %% "monocle-core"  % monocleV,
      "com.github.julien-truffaut" %% "monocle-macro" % monocleV,
      "co.fs2"                     %% "fs2-core"      % "3.0.0-M7",
      "co.fs2"                     %% "fs2-io"        % "3.0.0-M7"
    ),
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.5" % Test
  )

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.3" cross CrossVersion.full)
