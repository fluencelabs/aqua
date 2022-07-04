package aqua.plugin

import scalajs.js
import concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

case class Function(closure: js.Function)
case class Plugin(name: String, functions: List[Function])



object Plugin {
  def get(path: String): Future[js.Dynamic] = {
    for {
      a <- js.`import`[js.Dynamic](path).toFuture
      plugin <- a.applyDynamic("plugins")().asInstanceOf[js.Promise[js.Dynamic]].toFuture
      _ = {
        val b = plugin.asInstanceOf[js.Dictionary[js.Dictionary[js.Any]]]
        println(b.keys.toList)
        println(b.get(b.keys.toList.head).map(_.keys))
      }
      res <- plugin.ipfs.applyDynamic("log")().asInstanceOf[js.Promise[js.Dynamic]].toFuture
    } yield {
      res
    }
  }
}
