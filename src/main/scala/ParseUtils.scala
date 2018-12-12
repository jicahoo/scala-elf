import FileHeader.EndianessEnum

object ParseUtils {
  def asInt(bytetArray: Array[Byte], idx: Int, byteCnt: Int,
            endian: EndianessEnum.EndianessENum): Int = {
    assert(byteCnt <= 4)
    val sum = 0
    var idxs = (idx until (idx + byteCnt)).toList
    if (endian == EndianessEnum.BIG) {
      idxs = idxs.reverse
    }

    idxs.zipWithIndex.map(e => (bytetArray(e._1.toInt) & 0xff) << (8 * e._2)).sum
  }

  def asInt(byteArray: Array[Byte], offSetSizePair: OffSetSizePair, endian: EndianessEnum.EndianessENum): Int = {
    ParseUtils.asInt(byteArray, offSetSizePair.offSet, offSetSizePair.size, endian)
  }

  def asInt(byteArray: Array[Byte],
            globalOffSet: Int,
            offSetSizePair: OffSetSizePair,
            endian: EndianessEnum.EndianessENum): Int = {
    ParseUtils.asInt(byteArray, globalOffSet + offSetSizePair.offSet, offSetSizePair.size, endian)
  }
}
