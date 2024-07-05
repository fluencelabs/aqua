import BundleJS.*

val aquaVersion = "0.14.11"

val scalaV = "3.4.2"
val catsV = "2.12.0"
val catsParseV = "0.3.10"
val monocleV = "3.1.0"
val scalaTestV = "3.2.19"
val scalaTestScalaCheckV = "3.2.18.0"
val sourcecodeV = "0.4.2"
// Snapshot is used to get latest fixes
val fs2V = "3.10.2"
val catsEffectV = "3.6-1f95fd7"
val declineV = "2.3.0"
val circeVersion = "0.14.2"
val scribeV = "3.13.0"

name := "aqua-hll"

val orgName = "Fluence DAO"
val licenseStartYear = 2024
val licenseName = "AGPL-3.0-only"
val licenseUrl = "https://www.gnu.org/licenses/agpl-3.0.txt"

val license = HeaderLicense.AGPLv3Only(
  yyyy = licenseStartYear.toString,
  copyrightOwner = orgName,
  licenseStyle = HeaderLicenseStyle.Detailed
)

val licenseSettings = Seq(
  organizationName := orgName,
  startYear        := Some(licenseStartYear),
  licenses += (licenseName, new URI(licenseUrl).toURL()),
  headerLicense := Some(license)
)

licenseSettings

val commons = licenseSettings ++ Seq(
  version := {
    val aquaSnapshot = sys.env.getOrElse("SNAPSHOT", "")
    if (aquaSnapshot.isEmpty()) aquaVersion else aquaVersion + "-" + aquaSnapshot,
  },
  scalaVersion := scalaV,
  libraryDependencies ++= Seq(
    "com.outr"          %%% "scribe"          % scribeV,
    "dev.optics"        %%% "monocle-core"    % monocleV,
    "dev.optics"        %%% "monocle-macro"   % monocleV,
    "org.scalatest"     %%% "scalatest"       % scalaTestV           % Test,
    "org.scalatestplus" %%% "scalacheck-1-17" % scalaTestScalaCheckV % Test
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
  },
  // Needed to resolve snapshot versions
  resolvers ++= Resolver.sonatypeOssRepos("snapshots")
)

lazy val `aqua-run` = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("aqua-run"))
  .settings(commons)
  .dependsOn(compiler, `backend-air`, `backend-ts`, io, definitions, logging, constants)
  .enablePlugins(AutomateHeaderPlugin)

lazy val io = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(commons)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-effect" % catsEffectV,
      "co.fs2"        %%% "fs2-io"      % fs2V
    )
  )
  .dependsOn(compiler, parser, helpers)
  .enablePlugins(AutomateHeaderPlugin)

lazy val ioJS = io.js
  .settings(licenseSettings)
  .settings(
    scalaJSLinkerConfig ~= (
      _.withModuleKind(ModuleKind.CommonJSModule)
        .withJSHeader(s"/*${license.text}*/\n")
    )
  )
  .dependsOn(`js-imports`)
  .enablePlugins(AutomateHeaderPlugin)

lazy val `language-server-api` = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("language-server/language-server-api"))
  .settings(commons)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-effect" % catsEffectV,
      "co.fs2"        %%% "fs2-io"      % fs2V
    )
  )
  .dependsOn(compiler, io, compiler % "test->test")
  .enablePlugins(AutomateHeaderPlugin)

lazy val `language-server-apiJS` = `language-server-api`.js
  .settings(licenseSettings)
  .settings(
    scalaJSLinkerConfig ~= (
      _.withModuleKind(ModuleKind.CommonJSModule)
        .withJSHeader(s"/*${license.text}*/\n")
    ),
    scalaJSUseMainModuleInitializer := false
  )
  .settings(addBundleJS("../../language-server-npm/aqua-lsp-api.js"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(`js-exports`, `js-imports`)
  .enablePlugins(AutomateHeaderPlugin)

lazy val `js-exports` = project
  .in(file("js/js-exports"))
  .enablePlugins(ScalaJSPlugin)
  .settings(commons)
  .dependsOn(`backend`.js, definitions.js)
  .enablePlugins(AutomateHeaderPlugin)

lazy val `js-imports` = project
  .in(file("js/js-imports"))
  .enablePlugins(ScalaJSPlugin)
  .settings(commons)
  .dependsOn(`js-exports`, transform.js)

lazy val `aqua-api` = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("api/api"))
  .settings(commons)
  .dependsOn(`aqua-run`, `backend-api`)
  .enablePlugins(AutomateHeaderPlugin)

lazy val `aqua-apiJS` = `aqua-api`.js
  .settings(licenseSettings)
  .settings(
    scalaJSLinkerConfig ~= (
      _.withModuleKind(ModuleKind.ESModule)
        .withJSHeader(s"/*${license.text}*/\n")
    ),
    scalaJSUseMainModuleInitializer := true,
    Test / test                     := {}
  )
  .settings(addBundleJS("../../api-npm/aqua-api.js"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(`js-exports`)
  .enablePlugins(AutomateHeaderPlugin)

lazy val types = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(commons)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % catsV
    )
  )
  .dependsOn(errors)
  .enablePlugins(AutomateHeaderPlugin)

lazy val parser = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(commons)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-parse" % catsParseV,
      "org.typelevel" %%% "cats-free"  % catsV
    )
  )
  .dependsOn(types, helpers)
  .enablePlugins(AutomateHeaderPlugin)

lazy val linker = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(commons)
  .dependsOn(parser)
  .enablePlugins(AutomateHeaderPlugin)

lazy val tree = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("model/tree"))
  .settings(commons)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-free" % catsV
    )
  )
  .dependsOn(helpers)
  .enablePlugins(AutomateHeaderPlugin)

lazy val raw = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("model/raw"))
  .settings(commons)
  .dependsOn(types, tree)
  .enablePlugins(AutomateHeaderPlugin)

lazy val model = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(commons)
  .dependsOn(types, tree, raw, helpers, errors)
  .enablePlugins(AutomateHeaderPlugin)

lazy val res = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("model/res"))
  .settings(commons)
  .dependsOn(model)
  .enablePlugins(AutomateHeaderPlugin)

lazy val inline = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("model/inline"))
  .settings(commons)
  .dependsOn(raw, model, mangler)
  .enablePlugins(AutomateHeaderPlugin)

lazy val transform = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("model/transform"))
  .settings(commons)
  .dependsOn(model, res, inline, res % "test->test")
  .enablePlugins(AutomateHeaderPlugin)

lazy val semantics = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(commons)
  .dependsOn(raw, parser, errors, mangler)
  .enablePlugins(AutomateHeaderPlugin)

lazy val compiler = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("compiler"))
  .settings(commons)
  .dependsOn(semantics, linker, backend, transform % "test->test", res % "test->test")
  .enablePlugins(AutomateHeaderPlugin)

lazy val backend = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("backend"))
  .settings(commons)
  .enablePlugins(BuildInfoPlugin)
  .settings(
    buildInfoKeys    := Seq[BuildInfoKey](version),
    buildInfoPackage := "aqua.backend"
  )
  .dependsOn(res, definitions)
  .enablePlugins(AutomateHeaderPlugin)

lazy val definitions = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("backend/definitions"))
  .settings(commons)
  .settings(
    libraryDependencies ++= Seq(
      "io.circe" %%% "circe-core",
      "io.circe" %%% "circe-generic",
      "io.circe" %%% "circe-parser"
    ).map(_ % circeVersion)
  )
  .dependsOn(res, types)
  .enablePlugins(AutomateHeaderPlugin)

lazy val logging = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("utils/logging"))
  .settings(commons)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % catsV
    )
  )
  .enablePlugins(AutomateHeaderPlugin)

lazy val mangler = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("utils/mangler"))
  .settings(commons)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % catsV
    )
  )
  .enablePlugins(AutomateHeaderPlugin)

lazy val constants = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("utils/constants"))
  .settings(commons)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % catsV
    )
  )
  .dependsOn(parser, raw)
  .enablePlugins(AutomateHeaderPlugin)

lazy val helpers = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("utils/helpers"))
  .settings(commons)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % catsV,
      "org.typelevel" %%% "cats-free" % catsV
    )
  )
  .dependsOn(errors)
  .enablePlugins(AutomateHeaderPlugin)

lazy val errors = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("utils/errors"))
  .settings(commons)
  .settings(
    libraryDependencies ++= Seq(
      "com.lihaoyi" %%% "sourcecode" % sourcecodeV
    )
  )
  .enablePlugins(AutomateHeaderPlugin)

lazy val `backend-air` = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("backend/air"))
  .settings(commons)
  .dependsOn(backend, transform)
  .enablePlugins(AutomateHeaderPlugin)

lazy val `backend-api` = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("backend/api"))
  .settings(commons)
  .dependsOn(backend, transform, `backend-air`)
  .enablePlugins(AutomateHeaderPlugin)

lazy val `backend-ts` = crossProject(JVMPlatform, JSPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("backend/ts"))
  .settings(commons)
  .settings(
    libraryDependencies ++= Seq(
      "io.circe" %%% "circe-core",
      "io.circe" %%% "circe-generic",
      "io.circe" %%% "circe-parser"
    ).map(_ % circeVersion)
  )
  .dependsOn(`backend-air`, definitions)
  .enablePlugins(AutomateHeaderPlugin)
