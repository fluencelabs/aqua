import sbt.*
import sbt.Keys.*

import org.scalajs.linker.interface.Report
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.*

/**
 * Utility to add bundling js functionality to a project.
 */
object BundleJS {
  // Bundle full js (result of fullLinkJS)
  val fullBundleJS = taskKey[Unit]("Full bundle JS")
  // Bundle fast js (result of fastLinkJS)
  val fastBundleJS = taskKey[Unit]("Fast bundle JS")

  /**
   * Add full/fast bundle JS tasks to a project.
   *
   * @param outputFilePath **relative to baseDirectory** path to output file
   * @return Seq of settings with tasks
   */
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
