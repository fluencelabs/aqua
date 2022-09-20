val dottyVersion = "3.1.3"

scalaVersion := dottyVersion

val baseAquaVersion = settingKey[String]("base aqua version")

val catsV = "2.8.0"
val catsParseV = "0.3.8"
val monocleV = "3.1.0"
val scalaTestV = "3.2.10"
val fs2V = "3.2.11"
val catsEffectV = "3.3.14"
val declineV = "2.3.0"
val circeVersion = "0.14.2"
val scribeV = "3.7.1"

name := "aqua-hll"

val commons = Seq(
  baseAquaVersion := "0.7.6",
  version         := baseAquaVersion.value + "-" + sys.env.getOrElse("BUILD_NUMBER", "SNAPSHOT"),
  scalaVersion    := dottyVersion,
  libraryDependencies ++= Seq(
    "com.outr"      %%% "scribe"    % scribeV,
    "org.scalatest" %%% "scalatest" % scalaTestV % Test
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

lazy val cli = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("cli"))
  .settings(commons: _*)
  .settings(
    libraryDependencies ++= Seq(
      "com.monovore" %%% "decline"        % declineV,
      "com.monovore" %%% "decline-effect" % declineV
    )
  )
  .dependsOn(compiler, `backend-air`, `backend-ts`, io)

lazy val cliJS = cli.js
  .settings(
    scalaJSLinkerConfig             ~= (_.withModuleKind(ModuleKind.ESModule)),
    scalaJSUseMainModuleInitializer := true
  )

lazy val cliJVM = cli.jvm
  .settings(
    Compile / run / mainClass  := Some("aqua.AquaCli"),
    assembly / mainClass       := Some("aqua.AquaCli"),
    assembly / assemblyJarName := "aqua-" + version.value + ".jar",
    libraryDependencies ++= Seq(
    )
  )

lazy val io = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(commons: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-effect" % catsEffectV,
      "co.fs2"        %%% "fs2-io"      % fs2V
    )
  )
  .dependsOn(compiler, parser)

lazy val `language-server-api` = project
  .in(file("language-server-api"))
  .enablePlugins(ScalaJSPlugin)
  .settings(commons: _*)
  .settings(
    scalaJSLinkerConfig             ~= (_.withModuleKind(ModuleKind.CommonJSModule)),
    scalaJSUseMainModuleInitializer := true
  )
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-effect" % catsEffectV,
      "co.fs2"        %%% "fs2-io"      % fs2V
    )
  )
  .dependsOn(compiler.js, io.js)

lazy val types = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(commons)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % catsV
    )
  )

lazy val parser = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(commons: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-parse" % catsParseV,
      "org.typelevel" %%% "cats-free"  % catsV
    )
  )
  .dependsOn(types)

lazy val linker = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(commons: _*)
  .dependsOn(parser)

lazy val tree = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("model/tree"))
  .settings(commons: _*)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-free" % catsV
    )
  )

lazy val raw = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("model/raw"))
  .settings(commons: _*)
  .dependsOn(types, tree)

lazy val model = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(commons: _*)
  .dependsOn(types, tree, raw)

lazy val res = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("model/res"))
  .settings(commons: _*)
  .dependsOn(model)

lazy val inline = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("model/inline"))
  .settings(commons: _*)
  .dependsOn(raw, model)

lazy val transform = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("model/transform"))
  .settings(commons: _*)
  .dependsOn(model, res, inline)

lazy val semantics = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(commons: _*)
  .settings(
    libraryDependencies ++= Seq(
      "dev.optics" %%% "monocle-core"  % monocleV,
      "dev.optics" %%% "monocle-macro" % monocleV
    )
  )
  .dependsOn(raw, parser)

lazy val compiler = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("compiler"))
  .settings(commons: _*)
  .dependsOn(semantics, linker, backend, transform % Test)

lazy val backend = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("backend"))
  .settings(commons: _*)
  .enablePlugins(BuildInfoPlugin)
  .settings(
    buildInfoKeys    := Seq[BuildInfoKey](version),
    buildInfoPackage := "aqua.backend"
  )
  .dependsOn(res)

lazy val `backend-air` = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("backend/air"))
  .settings(commons: _*)
  .dependsOn(backend, transform)

lazy val `backend-ts` = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("backend/ts"))
  .settings(commons: _*)
  .settings(
    libraryDependencies ++= Seq(
      "io.circe" %%% "circe-core",
      "io.circe" %%% "circe-generic",
      "io.circe" %%% "circe-parser"
    ).map(_ % circeVersion)
  )
  .dependsOn(`backend-air`)
