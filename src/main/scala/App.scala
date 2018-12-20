import java.io.File

import com.jichao.Hello
import com.typesafe.scalalogging.Logger

import scala.reflect.runtime.{universe => ru}


object App {
  val logger = Logger(classOf[App])

  def main(args: Array[String]): Unit = {
    logger.info("Started")
    Hello.logDemo()
    val pwd = new File(".").getAbsolutePath
    logger.debug(pwd)
    val filePath = "src/main/resources/libxml2.so.2.9.1"
    val elfFile = new ElfFile(filePath)
    elfFile.printSummary()
    elfFile.printProgramHeaders()
    elfFile.printSectionHeaders()
  }

}

