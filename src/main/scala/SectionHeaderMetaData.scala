trait SectionHeaderMetaData {
  def shName: OffSetSizePair

  def shType: OffSetSizePair

  def shFlags: OffSetSizePair

  def shAddr: OffSetSizePair

  def shOffset: OffSetSizePair

  def shSize: OffSetSizePair

  def shLink: OffSetSizePair

  def shInfo: OffSetSizePair

  def shAddrAlign: OffSetSizePair

  def shEntSize: OffSetSizePair
}

class SectionHeaderMetaData32 extends SectionHeaderMetaData {
  //  type OFFSET = OffSetSizePair
  def shName: OffSetSizePair = OffSetSizePair(0x00, 0x04)

  def shType: OffSetSizePair = OffSetSizePair(0x04, 0x04)

  def shFlags: OffSetSizePair = OffSetSizePair(0x08, 0x04)

  def shAddr: OffSetSizePair = OffSetSizePair(0x0C, 0x04)

  def shOffset: OffSetSizePair = OffSetSizePair(0x10, 0x04)

  def shSize: OffSetSizePair = OffSetSizePair(0x14, 0x04)

  def shLink: OffSetSizePair = OffSetSizePair(0x18, 0x04)

  def shInfo: OffSetSizePair = OffSetSizePair(0x1C, 0x04)

  def shAddrAlign: OffSetSizePair = OffSetSizePair(0x20, 0x04)

  def shEntSize: OffSetSizePair = OffSetSizePair(0x24, 0x04)
}

