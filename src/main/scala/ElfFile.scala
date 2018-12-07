import java.nio.file.{Files, Paths}

import FileHeader._
import ProgramHeader._
import SectionHeader._

class ElfFile(val filePath: String) {
  def asInt(bytetArray: Array[Byte], idx: Int, byteCnt: Int,
            endian: EndianessEnum.EndianessENum): Int = {
    assert(byteCnt <= 4)
    val sum = 0
    var idxs = (idx until (idx + byteCnt)).toList
    if (endian == EndianessEnum.BIG) {
      idxs = idxs.reverse
    }

    idxs.zipWithIndex.map(e => (bytetArray(e._1.toInt) & 0xff) <<(8*e._2)).sum
  }

  def printSummary(): Unit = {
    val byteArray = Files.readAllBytes(Paths.get(filePath))
    println(s"Byte size: ${byteArray.length}")
    println(byteArray(0) == 0x7f)
    println(byteArray(1) == 'E'.toInt) //E
    println(byteArray(2) == 'L'.toInt) //L
    println(byteArray(3) == 'F'.toInt)
    val wordLen = byteArray(IdentMetaData.ET_CLASS.offSet) & 0xff
    println(WordSizeEnum.apply(wordLen))
    val endian = EndianessEnum.apply(byteArray(0x5))
    println(endian)
    println(OsEnum.apply(byteArray(0x7)))
    val elfType = ObjectFileTypeEnum.apply(asInt(byteArray, 0x10, 2, endian))
    println(elfType)
    assert(elfType == ObjectFileTypeEnum.ET_DYN)

    if (WordSizeEnum.BIT32 == WordSizeEnum.apply(wordLen)) {
      val progHdrOff = asInt(byteArray, 0x1C, 4, endian)
      println(progHdrOff)
      val phEntSize = asInt(byteArray, 0x2A, 2, endian)
      println(s"A Program Entry Size: $phEntSize")
      val phEntNum = asInt(byteArray, 0x2C, 2, endian)
      println(s"Program entry count: $phEntNum")
      val phType = asInt(byteArray, progHdrOff + 0x00, 4, endian)
      println(ProgHeaderTypeEnum.apply(phType))

      val sectHdrOff = asInt(byteArray, 0x20, 4, endian)

      println(s"Section Header Offset: ${sectHdrOff.toHexString}")
      val shEntSize = asInt(byteArray, 0x2E, 2, endian)
      val shEntNum = asInt(byteArray, 0x30, 2, endian)
      println(s"Section header size: $shEntSize, Section header number: $shEntNum ")


      // Find the section .shstrtab
      val strTabSectHeaderIdx = sectHdrOff + (shEntNum - 1) * shEntSize

      val shType = ShTypeEnum.apply(asInt(byteArray, strTabSectHeaderIdx + 0x4, 4, endian))
      println(shType)

      val sectionOffSet = asInt(byteArray, strTabSectHeaderIdx + 0x10, 4, endian)
      println(sectionOffSet)
      val sectionSize = asInt(byteArray, strTabSectHeaderIdx + 0x14, 4, endian)
      println(sectionSize)
      (sectionOffSet until (sectionOffSet + sectionSize)).foreach(
        i => {
          val c = byteArray(i)
          if (c == 0) {
            println()
          } else {
            print(byteArray(i).toChar)
          }
        }
      )
      println()
    }
  }
}
