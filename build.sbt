val dottyVersion = "2.13.5"

scalaVersion := dottyVersion

//val dottyVersion = "3.0.0-RC1"

val aquaV = "0.1.0"

val catsV = "2.4.2"
val catsParseV = "0.3.1"
val monocleV = "3.0.0-M3"
val scalaTestV = "3.2.5"
val fs2V = "3.0.0-M7"

name                        := "aqua-hll"
mainClass in (Compile, run) := Some("aqua.Main")
mainClass in assembly       := Some("aqua.Main")
assemblyJarName in assembly := "aqua-hll.jar"

val commons = Seq(
  version                                := aquaV,
  scalaVersion                           := dottyVersion,
  libraryDependencies += "org.scalatest" %% "scalatest" % scalaTestV % Test,
  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.3" cross CrossVersion.full)
)

lazy val cli = project
  .settings(commons: _*)
  .settings(
    libraryDependencies ++= Seq(
      "com.github.scopt" %% "scopt"       % "4.0.1",
      "org.typelevel"    %% "cats-effect" % "3.0.0-RC2",
      "co.fs2"           %% "fs2-core"    % fs2V,
      "co.fs2"           %% "fs2-io"      % fs2V
    )
  )
  .dependsOn(semantics, `backend-air`, `backend-ts`)

lazy val types = project
  .settings(commons)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % catsV
    )
  )

lazy val parser = project
  .settings(commons: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-parse" % catsParseV,
      "org.typelevel" %% "cats-free"  % catsV
    )
  )
  .dependsOn(types)

lazy val model = project
  .settings(commons: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-free" % catsV
    )
  )
  .dependsOn(types)

lazy val semantics = project
  .settings(commons: _*)
  .settings(
    libraryDependencies ++= Seq(
      "com.github.julien-truffaut" %% "monocle-core"  % monocleV,
      "com.github.julien-truffaut" %% "monocle-macro" % monocleV
    )
  )
  .dependsOn(model, parser)

lazy val `backend-air` = project
  .in(file("backend/air"))
  .settings(commons: _*)
  .dependsOn(model)

lazy val `backend-ts` = project
  .in(file("backend/ts"))
  .settings(commons: _*)
  .dependsOn(`backend-air`)
