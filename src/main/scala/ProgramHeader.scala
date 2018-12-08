import FileHeader.EndianessEnum.EndianessENum

abstract class ProgramHeader {

}

object ProgramHeader {

  object ProgHeaderTypeEnum extends Enumeration {
    type ProgHeaderTypeEnum = Value
    val PT_NULL, PT_LOAD, PT_DYNAMIC, PT_INTERP, PT_NOTE, PT_SHLIB, PT_PHDR = Value
    val PT_LOOS = Value(0x60000000)
  }

  def parse(byteArray: Array[Byte], phOffSet: Int, phEntSize: Int, phNum: Int, endian: EndianessENum): List[ProgramHeader] = {
    val phType = ParseUtils.asInt(byteArray, phOffSet + 0x00, 4, endian)
    println(ProgHeaderTypeEnum.apply(phType))
    null
  }
}