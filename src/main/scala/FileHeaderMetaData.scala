trait FileHeaderMetaData {
  def phOff: OffSetSizePair
  def phEntSize: OffSetSizePair
  def phNum: OffSetSizePair

  def shOff: OffSetSizePair
  def shEntSize: OffSetSizePair
  def shNum: OffSetSizePair
}

