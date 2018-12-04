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

  object ObjectFileTypeEnum extends Enumeration {
    type ObjecFileTypeEnum = Value
    //TODO: There are other values.
    val ET_NONE, ET_REL, ET_EXEC, ET_DYN, ET_CORE = Value
  }



  def asInt(bytetArray: Array[Byte], idx: Int, byteCnt: Int,
            endian: App.EndianessEnum.EndianessENum): Int = {
    assert(byteCnt <= 4)
    val sum = 0
    var idxs = (idx until (idx + byteCnt)).toList
    if (endian == App.EndianessEnum.BIG) {
      idxs = idxs.reverse
    }

    idxs.zipWithIndex.map(e => bytetArray(e._1.toInt).toInt<<(8*e._2)).sum
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
    val endian = EndianessEnum.apply(byteArray(0x5))
    println(endian)
    println(OsEnum.apply(byteArray(0x7)))
    val elfType = ObjectFileTypeEnum.apply(asInt(byteArray, 0x10, 2, endian))
    println(elfType)
    assert(elfType == ObjectFileTypeEnum.ET_DYN)
  }

}

