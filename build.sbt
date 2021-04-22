val dottyVersion = "2.13.5"

scalaVersion := dottyVersion

//val dottyVersion = "3.0.0-RC2"

val baseAquaVersion = settingKey[String]("base aqua version")

val catsV = "2.5.0"
val catsParseV = "0.3.2"
val monocleV = "3.0.0-M4"
val scalaTestV = "3.2.7"
val fs2V = "3.0.0"
val catsEffectV = "3.0.2"
val declineV = "2.0.0-RC1"

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
      "co.fs2"        %% "fs2-io"         % fs2V
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
