import FileHeader.EndianessEnum
import FileHeader.EndianessEnum.EndianessENum
import SectionHeader.ShFlagsEnum.ShFlagsEnum
import SectionHeader.{ShFlagsEnum, ShTypeEnum}
import SectionHeader.ShTypeEnum.ShTypeEnum
import com.typesafe.scalalogging.Logger

import scala.reflect.runtime.{universe => ru}

case class SectionHeader(
                          var shName: Int= 0,
                          var shType: ShTypeEnum= ShTypeEnum.SHT_NULL,
                          var shFlags: Int = 0,
                          var shAddr: Int = 0,
                          var shOffset: Int= 0,
                          var shSize: Int= 0,
                          var shLink: Int= 0,
                          var shInfo: Int= 0,
                          var shAddrAlign: Int= 0,
                          var shEntSize: Int =0
                        ) {
  def flags: Set[ShFlagsEnum] = ShFlagsEnum.flags(shFlags)
}

object SectionHeader {
  val logger = Logger(classOf[SectionHeader])

  object ShTypeEnum extends Enumeration {
    type ShTypeEnum = Value
    val SHT_NULL, SHT_PROGBITS, SHT_SYMTAB, SHT_STRTAB, SHT_RELA, SHT_HASH, SHT_DYNAMIC, SHT_NOTE = Value
    val SHT_NOBITS, SHT_REL, SHT_SHLIB, SHT_DYNSYM, SHT_INIT_ARRAY, SHT_FINI_ARRAY = Value
    val SHT_PREINIT_ARRAY, SHT_GROUP, SHT_NUM = Value
    val SHT_LOOS:ShTypeEnum = Value(0x60000000)
    val    SHT_GNU_ATTRIBUTES:ShTypeEnum = Value(0x6ffffff5) // Object attributes.
    val    SHT_GNU_HASH:ShTypeEnum = Value(0x6ffffff6)       // GNU-style hash table.
    val    SHT_GNU_verdef:ShTypeEnum = Value(0x6ffffffd)     // GNU version definitions.
    val    SHT_GNU_verneed:ShTypeEnum = Value(0x6ffffffe)    // GNU version references.
    val    SHT_GNU_versym:ShTypeEnum = Value(0x6fffffff)     // GNU symbol versions table.
  }
  object ShFlagsEnum extends Enumeration {
    type ShFlagsEnum = Value
    val SHF_UNKOWN:ShFlagsEnum = Value(0x0)

    val SHF_WRITE:ShFlagsEnum = Value(0x1)
    val SHF_ALLOC:ShFlagsEnum = Value(0x2)
    val SHF_EXECINSTR:ShFlagsEnum = Value(0x4)
    val SHF_MERGE:ShFlagsEnum = Value(0x10)
    val SHF_STRINGS:ShFlagsEnum = Value(0x20)
    val SHF_INFO_LINK:ShFlagsEnum = Value(0x40)
    val SHF_LINK_ORDER:ShFlagsEnum = Value(0x80)
    val SHF_OS_NONCONFORMING:ShFlagsEnum = Value(0x100)
    val SHF_GROUP:ShFlagsEnum = Value(0x200)
    val SHF_TLS:ShFlagsEnum = Value(0x400)
    val SHF_MASKOS:ShFlagsEnum = Value(0x0ff00000)
    val SHF_MASKPROC:ShFlagsEnum = Value(0xf0000000)
    val SHF_ORDERED:ShFlagsEnum = Value(0x4000000)
    val SHF_EXCLUDE:ShFlagsEnum = Value(0x8000000)

    def flags(intVal: Int): Set[ShFlagsEnum] = {
//      val x=values.filter(x => (x.id & intVal) != 0) it will be performance issue. Why Scala enumeration impl issue?

      (0 until 32).map(1 << _).filter(x => (x & intVal) != 0).map(apply).toSet
    }
  }

  def parse(byteArray: Array[Byte], phOffSet: Int, phEntSize: Int, phNum: Int, endian: EndianessENum): List[SectionHeader] = {
    (0 until phNum).map(phOffSet + _ * phEntSize).map(
      offSet => {
        logger.debug(s"========offSet: $offSet")
        val progHeaderMetaData = new SectionHeaderMetaData32
        val targetObjType  = ru.typeOf[SectionHeader]
        val targetObj = new SectionHeader()
        val metaDataType = ru.typeOf[SectionHeaderMetaData]
        ProgramHeader.progHeader(offSet, progHeaderMetaData, targetObj, byteArray, EndianessEnum.LITTLE,
          metaDataType,
          targetObjType
        )
        logger.debug(targetObj.toString)
        targetObj
      }
    ).toList
  }
}