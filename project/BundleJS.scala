import sbt.*
import sbt.Keys.*

import org.scalajs.linker.interface.Report
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.*

object BundleJS {
  val fullBundleJS = taskKey[Unit]("Full bundle JS")
  val fastBundleJS = taskKey[Unit]("Fast bundle JS")

  def addBundleJS(
    outputFilePath: String
  ) = Seq(
    fullBundleJS := Def.taskDyn {
      bundleJS(fullLinkJS, outputFilePath)
    }.value,
    fastBundleJS := Def.taskDyn {
      bundleJS(fastLinkJS, outputFilePath)
    }.value
  )

  private def bundleJS(
    linkJSTask: TaskKey[Attributed[Report]],
    outputFilePath: String
  ) = Def.taskDyn {
    val logger = streams.value.log

    val jsDir = (Compile / linkJSTask / scalaJSLinkerOutputDirectory).value
    val linkResult = (Compile / linkJSTask).value
    val outputFile = baseDirectory.value / outputFilePath

    linkResult.data.publicModules.toList match {
      case Nil =>
        throw new RuntimeException("No public modules generated")
      case _ :: _ :: _ =>
        throw new RuntimeException("More than one public module generated")
      case module :: Nil =>
        val jsFile = jsDir / module.jsFileName
        Def.task {
          logger.info(s"Copying $jsFile to $outputFile")
          IO.copyFile(jsFile, outputFile)
        }
    }
  }
}
