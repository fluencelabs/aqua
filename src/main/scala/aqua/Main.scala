package aqua

import aqua.Aqua.`parser`
import cats.effect.{IO, IOApp}
import cats.syntax.show._
import cats.Show
import aqua.ir._
import cats.data.{Kleisli, NonEmptyList, Validated, ValidatedNel}
import cats.parse.LocationMap
import cats.syntax.validated._

object Main extends IOApp.Simple {

  override def run: IO[Unit] =
    IO {

      val air: Air = Air.Fold(DataView.VarLens("iter", "$.all!"), "label", Air.Par(Air.Next("label"), Air.Null))

      println(air.show)

      type Call[F[_]] = Kleisli[F, List[String], Option[String]]

      val typeStr = """-- This is some data
                      |-- With a comment on first lines
                      |data Smth:
                      |  a: B
                      |  c: []D
                      |  oloolo: Akaka""".stripMargin

      val serviceStr = """service MySrv:
                       func: A -> i32
                       -- We can have comments inside
                       pure: -> []Cdef"""

      val funcStr = """func do_smth( a: X, b: -> Z ): -- And comments after the line
                      |  b()
                      |  x(a)""".stripMargin

      val funcStr2 = """func do_smth( a: X, b: -> Z ): -- And comments after the line
                       |  b()
                       |  b(a, z)""".stripMargin

      val aliasStr = """alias Akaka : i32"""

      def tryParse(str: String) =
        Aqua.parse(str) match {
          case Validated.Valid(v) ⇒ println(v)
          case Validated.Invalid(errs) ⇒
            errs.map(_.showForConsole(str)).map(println)
        }

      assert(Aqua.`parser` ne null)

      tryParse(typeStr)
      tryParse(serviceStr)
      tryParse(funcStr)
      tryParse(funcStr2)
      tryParse(aliasStr)
      tryParse((funcStr :: serviceStr :: typeStr :: aliasStr :: Nil).reverse.mkString("\n \n"))

    }

}
