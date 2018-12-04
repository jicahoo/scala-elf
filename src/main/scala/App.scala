import java.io.File
import java.nio.file.{Files, Paths}

object App {


  object WordSizeEnum extends Enumeration {
    type WordSizeEnum = Value
    val UNKNOWN, BIT32, BIT64 = Value
  }

  object EndianessEnum extends Enumeration {
    type EndianessENum = Value
    val UNKNOWN, LITTLE, BIG = Value
  }

  object OsEnum extends Enumeration {
    type OsEnum = Value
    //TODO: There are many other platforms
    val Unknown, SystemV, HP_UX, NetBSD, Linux = Value
  }


  def main(args: Array[String]): Unit = {
    println("Hello")
    var pwd = new File(".").getAbsolutePath
    println(pwd)
    val filePath = "src/main/resources/libxml2.so.2.9.1"
    val byteArray = Files.readAllBytes(Paths.get(filePath))
    println(s"Byte size: ${byteArray.length}")
    println(byteArray(0) == 0x7f)
    println(byteArray(1) == 'E'.toInt) //E
    println(byteArray(2) == 'L'.toInt) //L
    println(byteArray(3) == 'F'.toInt)
    val wordLen = byteArray(0x04)
    println(WordSizeEnum.apply(wordLen))
    println(EndianessEnum.apply(byteArray(0x5)))
    println(OsEnum.apply(byteArray(0x7)))
  }

}

