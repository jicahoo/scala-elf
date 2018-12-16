import java.nio.file.{Files, Paths}

import FileHeader._
import SectionHeader._
import com.typesafe.scalalogging.Logger

import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.{universe => ru}


class ElfFile(val filePath: String) {
  val logger = Logger(classOf[ElfFile])
  private var _endian = EndianessEnum.LITTLE
  private var _byteArray: Array[Byte] = _


  //Parsed data structure
  private var _fileHeader: FileHeader = _
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


    logger.debug(s"Byte size: ${_byteArray.length}")
    val magicNum = Array[Byte](0x7f.toByte, 'E'.toByte, 'L'.toByte, 'F'.toByte)
    if (!_byteArray.slice(0, 4).sameElements(magicNum)) {
      throw new IllegalArgumentException("Magic number is not '.ELF'.Maybe not a ELF file.")
    }

    val wordLenTag = asInt(IdentMetaData.ET_CLASS)
    val wordLen = WordSizeEnum.apply(wordLenTag)
    val endian = EndianessEnum.apply(asInt(IdentMetaData.ET_DATA))
    _endian = endian
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
        shOffSet = sectHdrOff,
        shEntSize = shEntSize,
        shNum = shEntNum
      )

      _fileHeader = fileHeader
      logger.debug(_fileHeader.toString)

      _phHeaders = ProgramHeader.parse(_byteArray,
        _fileHeader.phOffSet,
        _fileHeader.phEntSize,
        _fileHeader.phNum,
        _endian
      )

      _shHeaders = SectionHeader.parse(_byteArray, sectHdrOff, shEntSize, shEntNum, endian)
      _shHeaders.foreach(x => logger.debug(x.flags.map(_.toString).mkString("|")))


      // Find the section .shstrtab
      val strTabSectHeaderIdx = sectHdrOff + (shEntNum - 1) * shEntSize

      val shType = ShTypeEnum.apply(ParseUtils.asInt(_byteArray, strTabSectHeaderIdx + 0x4, 4, endian))
      logger.debug(shType.toString)

      val sectionOffSet = ParseUtils.asInt(_byteArray, strTabSectHeaderIdx + 0x10, 4, endian)
      logger.debug(sectionOffSet.toString)
      val sectionSize = ParseUtils.asInt(_byteArray, strTabSectHeaderIdx + 0x14, 4, endian)
      logger.debug(sectionSize.toString)
      (sectionOffSet until (sectionOffSet + sectionSize)).foreach(
        i => {
          val c = _byteArray(i)
          if (c == 0) {
            logger.debug("\n")
          } else {
            logger.debug(_byteArray(i).toChar.toString)
          }
        }
      )
      logger.debug("\n")
    }

  }


  def printProgramHeaders(): Unit = {
    val getters = ru.typeOf[ProgramHeader].decls.filter(_.isMethod).map(_.asMethod).filter(_.isGetter)
    val colNum = getters.size
    var maxes = ListBuffer.fill(colNum)(0)
    //TODO: The format algorithm is not efficient. But enough for now. May improve it in future.
    val geterNames = getters.map(_.name.toString.trim)
    geterNames.zipWithIndex.foreach(
      x => {
        if (x._1.length > maxes(x._2)) {
          maxes.update(x._2, x._1.length)
        }
      }
    )

    val classMirror = ru.runtimeMirror(getClass.getClassLoader)

    _phHeaders.foreach(
      x => {
        getters.zipWithIndex.foreach(
          y => {
            val obj = classMirror.reflect(x)
            val result = obj.reflectMethod(y._1)()
            val newLen = result match {
              case i: Int =>
                s"0x${i.toHexString}".length
              case _ =>
                result.toString.length
            }
            if (newLen > maxes(y._2)) {
              maxes.update(y._2, newLen)
            }
          }
        )
      }
    )
    maxes = maxes.map(_+3)

    geterNames.zipWithIndex.foreach(
      x => {
        val tailSpacesNum = maxes(x._2) - x._1.length
        val tailSpaces = List.fill(tailSpacesNum)(" ").mkString
        print(s"${x._1}$tailSpaces")
      }
    )
    println()
    println()

    _phHeaders.foreach(
      x => {
        getters.zipWithIndex.foreach(
          y => {
            val obj = classMirror.reflect(x)
            val result = obj.reflectMethod(y._1)()
            val (resultStr, len) = result match {
              case i: Int =>
                (s"0x${i.toHexString}", s"0x${i.toHexString}".length)
              case _ =>
                (result.toString, result.toString.length)
            }
            val tailSpaces = List.fill(maxes(y._2) - len)(" ").mkString
            print(s"$resultStr$tailSpaces")
          }
        )
        println()
      }
    )

  }
}
