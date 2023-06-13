package aqua.compiler

import org.graalvm.nativeimage.IsolateThread
import org.graalvm.nativeimage.c.function.CEntryPoint
import org.graalvm.nativeimage.c.`type`.{CCharPointer, CCharPointerPointer, CTypeConversion}

import scala.annotation.static

import cats.effect.unsafe.implicits.global

import aqua.api.{APICompilation, AquaAPIConfig}
import aqua.backend.api.APIBackend

// This is neede for @static to work in object
class Library {}

object Library {

  @CEntryPoint(name = "compile")
  @static
  def compile(
    thread: IsolateThread,
    codePointer: CCharPointer,
    resultPointer: CCharPointerPointer,
    errorsPointer: CCharPointerPointer
  ): Int = {
    val code = CTypeConversion.toJavaString(codePointer)

    val result = APICompilation
      .compileString(
        code,
        imports = Nil,
        aquaConfig = AquaAPIConfig(),
        backend = APIBackend
      )
      .unsafeRunSync()

    result.fold(
      errors =>
        errors.toChain.toList.zipWithIndex.foreach { case (error, i) =>
          errorsPointer.write(i, CTypeConversion.toCString(error).get())
        }

        1
      ,
      compiled =>
        compiled.toList.flatMap(_.compiled).flatMap(_.air).map(_.air).zipWithIndex.foreach {
          case (air, i) => resultPointer.write(i, CTypeConversion.toCString(code).get())
        }

        0
    )
  }

  // Without main native-image refuses to work
  @static
  def main(args: Array[String]): Unit = ()

}
