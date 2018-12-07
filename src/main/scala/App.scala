import java.io.File
import java.nio.file.{Files, Paths}


object App {

  def main(args: Array[String]): Unit = {
    println("Hello")
    var pwd = new File(".").getAbsolutePath
    println(pwd)
    val filePath = "src/main/resources/libxml2.so.2.9.1"
    val elfFile = new ElfFile(filePath)
    elfFile.printSummary()
  }

}

