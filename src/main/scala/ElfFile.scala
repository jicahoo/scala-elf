import java.nio.file.{Files, Paths}

import FileHeader._
import ProgramHeader._
import SectionHeader._

class ElfFile(val filePath: String) {
  private var _endian = EndianessEnum.LITTLE
  private def asInt(bytetArray: Array[Byte], idx: Int, byteCnt: Int,
            endian: EndianessEnum.EndianessENum): Int = {
    assert(byteCnt <= 4)
    val sum = 0
    var idxs = (idx until (idx + byteCnt)).toList
    if (endian == EndianessEnum.BIG) {
      idxs = idxs.reverse
    }

    idxs.zipWithIndex.map(e => (bytetArray(e._1.toInt) & 0xff) <<(8*e._2)).sum
  }

  def asInt(byteArray: Array[Byte], offSetSizePair: OffSetSizePair): Int = {
     if (offSetSizePair.size == 1) {
       byteArray(offSetSizePair.offSet) & 0xff
     } else {
       asInt(byteArray, offSetSizePair.offSet, offSetSizePair.size, _endian)
     }
  }

  def asInt(byteArray: Array[Byte], offSetSizePair: OffSetSizePair, endian: EndianessEnum.EndianessENum): Int = {
    asInt(byteArray, offSetSizePair.offSet, offSetSizePair.size, endian)
  }

  def printSummary(): Unit = {
    val byteArray = Files.readAllBytes(Paths.get(filePath))


    println(s"Byte size: ${byteArray.length}")
    println(byteArray(0) == 0x7f)
    println(byteArray(1) == 'E'.toInt) //E
    println(byteArray(2) == 'L'.toInt) //L
    println(byteArray(3) == 'F'.toInt)
    val wordLenTag = asInt(byteArray, IdentMetaData.ET_CLASS)
    val wordLen = WordSizeEnum.apply(wordLenTag)
    println(wordLen)
    val endian = EndianessEnum.apply(asInt(byteArray, IdentMetaData.ET_DATA))
    _endian  = endian
    println(endian)
    val osType = OsEnum.apply(asInt(byteArray, IdentMetaData.ET_OSABI))
    println(osType)
    val elfType = ObjectFileTypeEnum.apply(asInt(byteArray, FileHeader.elfType))
    println(elfType)

    assert(elfType == ObjectFileTypeEnum.ET_DYN)
    var metaData: MetaData = null;
     wordLen match {
      case WordSizeEnum.BIT32 => metaData = new MetaData32
      case WordSizeEnum.BIT64 => metaData = new MetaData64
    }

    if (WordSizeEnum.BIT32 == wordLen) {
      val progHdrOff = asInt(byteArray, metaData.phOff)
      println(progHdrOff)
      val phEntSize = asInt(byteArray, metaData.phEntSize)
      val phEntNum = asInt(byteArray, metaData.phNum)

      val phType = asInt(byteArray, progHdrOff + 0x00, 4, endian)
      println(ProgHeaderTypeEnum.apply(phType))

      val sectHdrOff = asInt(byteArray, metaData.shOff)

      val shEntSize = asInt(byteArray, metaData.shEntSize)
      val shEntNum = asInt(byteArray, metaData.shNum)

      //TODO: Builder pattern? better?
      val fileHeader = FileHeader(
        wordLen = wordLen,
        endian = endian,
        osType = osType,
        objFileType = elfType,
        phOffSet = progHdrOff,
        phEntSize = phEntSize,
        phNum = phEntNum,
        shOffSet =  sectHdrOff,
        shEntSize = shEntSize,
        shNum = shEntNum
      )

      println(fileHeader)

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
