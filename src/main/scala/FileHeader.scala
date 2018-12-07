class FileHeader {

}

object FileHeader {
  object WordSizeEnum extends Enumeration {
    type WordSizeEnum = Value
    val UNKNOWN, BIT32, BIT64 = Value
  }

  object EndianessEnum extends Enumeration {
    type EndianessENum = Value
    val UNKNOWN, LITTLE, BIG = Value
  }

  object OsEnum extends Enumeration {
    type OsEnum = Value
    //TODO: There are many other platforms
    val SystemV, HP_UX, NetBSD, Linux = Value
  }

  object ObjectFileTypeEnum extends Enumeration {
    type ObjecFileTypeEnum = Value
    //TODO: There are other values.
    val ET_NONE, ET_REL, ET_EXEC, ET_DYN, ET_CORE = Value
  }

  val elfType = OffSetSizePair(0x10, 2)
}