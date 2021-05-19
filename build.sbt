val dottyVersion = "2.13.5"

//val dottyVersion = "3.0.0-RC3"

scalaVersion := dottyVersion

val baseAquaVersion = settingKey[String]("base aqua version")

val catsV = "2.6.0"
val catsParseV = "0.3.3"
val monocleV = "3.0.0-M5"
val scalaTestV = "3.2.7" // TODO update version for scala 3-RC3
val fs2V = "3.0.2"
val catsEffectV = "3.1.0"
val declineV = "2.0.0-RC1" // Scala3 issue: https://github.com/bkirwi/decline/issues/260

name := "aqua-hll"

val commons = Seq(
  baseAquaVersion                        := "0.1.1",
  version                                := baseAquaVersion.value + "-" + sys.env.getOrElse("BUILD_NUMBER", "SNAPSHOT"),
  scalaVersion                           := dottyVersion,
  libraryDependencies += "org.scalatest" %% "scalatest" % scalaTestV % Test,
  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.3" cross CrossVersion.full)
)

commons

lazy val cli = project
  .settings(commons: _*)
  .settings(
    Compile / run / mainClass  := Some("aqua.AquaCli"),
    assembly / mainClass       := Some("aqua.AquaCli"),
    assembly / assemblyJarName := "aqua-cli-" + version.value + ".jar",
    libraryDependencies ++= Seq(
      "com.monovore"  %% "decline"        % declineV,
      "com.monovore"  %% "decline-effect" % declineV,
      "org.typelevel" %% "cats-effect"    % catsEffectV,
      "co.fs2"        %% "fs2-core"       % fs2V,
      "co.fs2"        %% "fs2-io"         % fs2V,
      "org.typelevel" %% "log4cats-slf4j" % "2.1.1"
    )
  )
  .dependsOn(semantics, `backend-air`, `backend-ts`, linker)

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

lazy val linker = project
  .settings(commons: _*)
  .settings(
  )
  .dependsOn(parser)

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
