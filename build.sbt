val dottyVersion = "3.0.1"

scalaVersion := dottyVersion

val baseAquaVersion = settingKey[String]("base aqua version")

val catsV = "2.6.1"
val catsParseV = "0.3.4"
val monocleV = "3.0.0-M6"
val scalaTestV = "3.2.9"
val fs2V = "3.0.6"
val catsEffectV = "3.2.1"
val airframeLogV = "21.5.4"
val log4catsV = "2.1.1"
val slf4jV = "1.7.30"
val declineV = "2.1.0"

val airframeLog = "org.wvlet.airframe" %% "airframe-log" % airframeLogV
val catsEffect = "org.typelevel"       %% "cats-effect"  % catsEffectV
val fs2Io = "co.fs2"                   %% "fs2-io"       % fs2V
val catsFree = "org.typelevel"         %% "cats-free"    % catsV
val cats = "org.typelevel"             %% "cats-core"    % catsV

name := "aqua-hll"

val commons = Seq(
  baseAquaVersion := "0.1.12",
  version         := baseAquaVersion.value + "-" + sys.env.getOrElse("BUILD_NUMBER", "SNAPSHOT"),
  scalaVersion    := dottyVersion,
  libraryDependencies ++= Seq(
    "org.typelevel" %% "log4cats-core" % log4catsV,
    airframeLog,
    "org.scalatest" %% "scalatest" % scalaTestV % Test
  ),
  scalacOptions ++= {
    Seq(
      "-encoding",
      "UTF-8",
      "-feature",
      "-language:implicitConversions",
      "-unchecked",
      "-Ykind-projector"
//      "-Xfatal-warnings"
    )
  }
)

commons

lazy val cli = project
  .settings(commons: _*)
  .settings(
    Compile / run / mainClass  := Some("aqua.AquaCli"),
    assembly / mainClass       := Some("aqua.AquaCli"),
    assembly / assemblyJarName := "aqua-cli-" + version.value + ".jar",
    libraryDependencies ++= Seq(
      "com.monovore" %% "decline"        % declineV,
      "com.monovore" %% "decline-effect" % declineV,
      catsEffect,
      fs2Io,
      "org.typelevel" %% "log4cats-slf4j" % log4catsV,
      "org.slf4j"      % "slf4j-jdk14"    % slf4jV
    )
  )
  .dependsOn(compiler, `backend-air`, `backend-ts`, `backend-js`)

lazy val types = project
  .settings(commons)
  .settings(
    libraryDependencies ++= Seq(
      cats
    )
  )

lazy val parser = project
  .settings(commons: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-parse" % catsParseV,
      catsFree
    )
  )
  .dependsOn(types)

lazy val linker = project
  .settings(commons: _*)
  .settings(
    libraryDependencies ++= Seq(
      airframeLog
    )
  )
  .dependsOn(parser)

lazy val model = project
  .settings(commons: _*)
  .settings(
    libraryDependencies ++= Seq(
      catsFree
    )
  )
  .dependsOn(types)

lazy val `test-kit` = project
  .in(file("model/test-kit"))
  .settings(commons: _*)
  .dependsOn(model)

lazy val semantics = project
  .settings(commons: _*)
  .settings(
    libraryDependencies ++= Seq(
      "com.github.julien-truffaut" %% "monocle-core"  % monocleV,
      "com.github.julien-truffaut" %% "monocle-macro" % monocleV
    )
  )
  .dependsOn(model, `test-kit` % Test, parser)

lazy val compiler = project
  .in(file("compiler"))
  .settings(commons: _*)
  .dependsOn(semantics, linker, backend)

lazy val backend = project
  .in(file("backend"))
  .settings(commons: _*)
  .dependsOn(model)

lazy val `backend-air` = project
  .in(file("backend/air"))
  .settings(commons: _*)
  .dependsOn(backend)

lazy val `backend-ts` = project
  .in(file("backend/ts"))
  .settings(commons: _*)
  .dependsOn(`backend-air`)

lazy val `backend-js` = project
  .in(file("backend/js"))
  .settings(commons: _*)
  .dependsOn(`backend-air`)
