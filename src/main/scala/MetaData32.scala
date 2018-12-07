class MetaData32 extends MetaData {

  //
  override def phOff: OffSetSizePair = OffSetSizePair(0x1C, 4)

  override def phEntSize: OffSetSizePair = OffSetSizePair(0x2A, 2)

  override def phNum: OffSetSizePair = OffSetSizePair(0x2C, 2)

  override def shOff: OffSetSizePair = OffSetSizePair(0x20, 4)

  override def shEntSize: OffSetSizePair = OffSetSizePair(0x2E, 2)

  override def shNum: OffSetSizePair = OffSetSizePair(0x30, 2)
}
