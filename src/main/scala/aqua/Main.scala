package aqua

import aqua.Aqua.`parser`
import cats.effect.{IO, IOApp}
import cats.syntax.show._
import cats.Show
import aqua.ir._
import cats.data.{Kleisli, NonEmptyList, Validated, ValidatedNel}
import cats.parse.LocationMap

object Main extends IOApp.Simple {

  override def run: IO[Unit] =
    IO {

      val air: Air = Air.Fold(DataView.VarLens("iter", "$.all!"), "label", Air.Par(Air.Next("label"), Air.Null))

      println(air.show)


      type Call[F[_]] = Kleisli[F, List[String], Option[String]]


      val typeStr = """data Smth:
                      |  a: B
                      |  c: []D
                      |  oloolo: Akaka""".stripMargin

      val serviceStr = """service MySrv:
                       func: A -> i32
                       pure: -> []Cdef"""

      val funcStr = """func do_smth( a: X, b: -> Z ):
                      |  line1
                      |  line2""".stripMargin

      def tryParse(str: String) =
        Aqua.`parser`.parseAll(str) match {
          case Right(v) ⇒ println(v)
          case Left(err) ⇒
            System.err.println(err.expected)
            System.err.println(str.substring(err.failedAtOffset))
            System.err.println(Console.BLUE + "===========" + Console.RESET)
        }

      assert(Aqua.`parser` ne null)

      tryParse(typeStr)
      tryParse(serviceStr)
      tryParse(funcStr)
      tryParse((funcStr :: serviceStr :: typeStr :: Nil).reverse.mkString("\n \n"))

    }

}

