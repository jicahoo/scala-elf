import java.nio.file.{Files, Paths}

import FileHeader._
import ProgramHeader._
import SectionHeader._

class ElfFile(val filePath: String) {
  private var _endian = EndianessEnum.LITTLE
  private var _byteArray: Array[Byte] = _


  //Parsed data structure
  private var _fileHeader:FileHeader = _
  private var _phHeaders: List[ProgramHeader] = _
  private var _shHeaders: List[SectionHeader] = _

  private def asInt(offSetSizePair: OffSetSizePair): Int = {
    asInt(_byteArray, offSetSizePair)
  }


  def asInt(byteArray: Array[Byte], offSetSizePair: OffSetSizePair): Int = {
     if (offSetSizePair.size == 1) {
       byteArray(offSetSizePair.offSet) & 0xff
     } else {
       ParseUtils.asInt(byteArray, offSetSizePair.offSet, offSetSizePair.size, _endian)
     }
  }

  def fileHeader: FileHeader = _fileHeader

  def printSummary(): Unit = {
    _byteArray = Files.readAllBytes(Paths.get(filePath))


    println(s"Byte size: ${_byteArray.length}")
    val magicNum = Array[Byte](0x7f.toByte, 'E'.toByte, 'L'.toByte, 'F'.toByte)
    if (!_byteArray.slice(0,4).sameElements(magicNum)) {
      throw new IllegalArgumentException("Magic number is not '.ELF'.Maybe not a ELF file.")
    }

    val wordLenTag = asInt(IdentMetaData.ET_CLASS)
    val wordLen = WordSizeEnum.apply(wordLenTag)
    val endian = EndianessEnum.apply(asInt(IdentMetaData.ET_DATA))
    _endian  = endian
    val osType = OsEnum.apply(asInt(IdentMetaData.ET_OSABI))
    val elfType = ObjectFileTypeEnum.apply(asInt(FileHeader.elfType))

    assert(elfType == ObjectFileTypeEnum.ET_DYN)
    var metaData: MetaData = null
     wordLen match {
      case WordSizeEnum.BIT32 => metaData = new MetaData32
      case WordSizeEnum.BIT64 => metaData = new MetaData64
    }

    if (WordSizeEnum.BIT32 == wordLen) {
      val progHdrOff = asInt(metaData.phOff)
      val phEntSize = asInt(metaData.phEntSize)
      val phEntNum = asInt(metaData.phNum)

      val sectHdrOff = asInt(metaData.shOff)
      val shEntSize = asInt(metaData.shEntSize)
      val shEntNum = asInt(metaData.shNum)

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

      _fileHeader = fileHeader
      println(_fileHeader)

      _phHeaders = ProgramHeader.parse(_byteArray,
        _fileHeader.phOffSet,
        _fileHeader.phEntSize,
        _fileHeader.phNum,
        _endian
      )


      // Find the section .shstrtab
      val strTabSectHeaderIdx = sectHdrOff + (shEntNum - 1) * shEntSize

      val shType = ShTypeEnum.apply(ParseUtils.asInt(_byteArray, strTabSectHeaderIdx + 0x4, 4, endian))
      println(shType)

      val sectionOffSet = ParseUtils.asInt(_byteArray, strTabSectHeaderIdx + 0x10, 4, endian)
      println(sectionOffSet)
      val sectionSize = ParseUtils.asInt(_byteArray, strTabSectHeaderIdx + 0x14, 4, endian)
      println(sectionSize)
      (sectionOffSet until (sectionOffSet + sectionSize)).foreach(
        i => {
          val c = _byteArray(i)
          if (c == 0) {
            println()
          } else {
            print(_byteArray(i).toChar)
          }
        }
      )
      println()
    }
  }
}
