import scala.reflect.runtime.{universe => ru}


abstract class ProgramHeaderMetaData {
  def pType: OffSetSizePair

  def pFlags: OffSetSizePair

  def pOffset: OffSetSizePair

  def pVirtualAddr: OffSetSizePair

  def pPhysicalAddr: OffSetSizePair

  def pFileSize: OffSetSizePair

  def pMemSize: OffSetSizePair

  def pAlign: OffSetSizePair
}

class ProgramHeaderMetaData32 extends ProgramHeaderMetaData {
  override def pType: OffSetSizePair = OffSetSizePair(0x00, 4)

  override def pOffset: OffSetSizePair = OffSetSizePair(0x04, 4)

  override def pVirtualAddr: OffSetSizePair = OffSetSizePair(0x08, 4)

  override def pPhysicalAddr: OffSetSizePair = OffSetSizePair(0x0C, 4)

  override def pFileSize: OffSetSizePair = OffSetSizePair(0x10, 4)

  override def pMemSize: OffSetSizePair = OffSetSizePair(0x14, 4)

  override def pFlags: OffSetSizePair = OffSetSizePair(0x18, 4)

  override def pAlign: OffSetSizePair = OffSetSizePair(0x1C, 4)
}

object ProgramHeaderMetaData32 {
  def main(args: Array[String]): Unit = {
    val classMirror = ru.runtimeMirror(getClass.getClassLoader)
    val progHeaderMetaData = new ProgramHeaderMetaData32
    val classTest = classMirror.reflect(progHeaderMetaData)
    val typeOfProgHeaderMetaData = ru.typeOf[ProgramHeaderMetaData32]
    val methods = typeOfProgHeaderMetaData.decls
    val methodNames = methods
      .filter(m => m.isMethod && !m.isConstructor)
      .map(_.asMethod.name.toString)

    methods
      .filter(m => m.isMethod && !m.isConstructor)
      .foreach(
        x => {
          val y = classTest.reflectMethod(x.asMethod)()
          println(y.asInstanceOf[OffSetSizePair].offSet)
        }
      )
    println(methodNames)
  }
}