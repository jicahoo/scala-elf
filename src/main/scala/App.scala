import java.io.File
import java.nio.file.{Files, Paths}

object App {
  def main(args: Array[String]): Unit = {
    println("Hello")
    var pwd = new File(".").getAbsolutePath
    println(pwd)
    val byteArray = Files.readAllBytes(Paths.get("libssl.so.1.0.2"))
    println(byteArray(0) == 0x7f)
    println(byteArray(1) == 'E'.toInt) //E
    println(byteArray(2) == 'L'.toInt) //L
    println(byteArray(3) == 'F'.toInt)

  }

}

