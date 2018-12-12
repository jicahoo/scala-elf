
abstract class SectionHeader {

}

object SectionHeader {

  object ShTypeEnum extends Enumeration {
    type ShTypeEnum = Value
    val SHT_NULL, SHT_PROGBITS, SHT_SYMTAB, SHT_STRTAB, SHT_RELA, SHT_HASH, SHT_DYNAMIC, SHT_NOTE = Value
    val SHT_NOBITS, SHT_REL, SHT_SHLIB, SHT_DYNSYM, SHT_INIT_ARRAY, SHT_FINI_ARRAY = Value
    val SHT_PREINIT_ARRAY, SHT_GROUP, SHT_NUM = Value
    val SHT_LOOS = Value(0x60000000)
  }

}