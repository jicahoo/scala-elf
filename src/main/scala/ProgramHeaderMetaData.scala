import java.time.OffsetDateTime

abstract class ProgramHeaderMetaData {
  def pType: OffSetSizePair
  def pFlags: OffSetSizePair
  def pOffset: OffSetSizePair
  def pVirtualAddr: OffSetSizePair
  def pPhysicalAddr: OffSetSizePair
  def pFileSize: OffSetSizePair
  def pMemSize: OffSetSizePair
  def pALgin: OffSetSizePair

}

class ProgramHeaderMetaData32 extends ProgramHeaderMetaData {
  override def pType: OffSetSizePair = OffSetSizePair(0x00, 4)

  override def pOffset: OffSetSizePair = OffSetSizePair(0x04, 4)

  override def pVirtualAddr: OffSetSizePair = OffSetSizePair(0x08, 4)

  override def pPhysicalAddr: OffSetSizePair = OffSetSizePair(0x0C, 4)

  override def pFileSize: OffSetSizePair = OffSetSizePair(0x10, 4)

  override def pMemSize: OffSetSizePair = OffSetSizePair(0x14, 4)

  override def pFlags: OffSetSizePair = OffSetSizePair(0x18, 4)

  override def pALgin: OffSetSizePair = OffSetSizePair(0x1C, 4)

}