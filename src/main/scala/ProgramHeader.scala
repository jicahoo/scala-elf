import java.nio.file.{Files, Paths}

import App.Good
import FileHeader.EndianessEnum
import FileHeader.EndianessEnum.EndianessENum
import ProgramHeader.ProgHeaderTypeEnum.ProgHeaderTypeEnum

import scala.reflect.ClassTag
import scala.reflect.runtime.{universe => ru}


case class ProgramHeader(
                          var pType: ProgHeaderTypeEnum = ProgramHeader.ProgHeaderTypeEnum.PT_NULL,
                          var pOffset: Int = 0,
                          var pVirtualAddr: Int = 0,
                          var pPhysicalAddr: Int = 0,
                          var pMemSize: Int = 0,
                          var pFileSize: Int = 0,
                          var pFlags: Int = 0,
                          var pAlign: Int = 0
                        )


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

  def setVal[T](a: Any, fieldName: String, strVal: T): Unit = {
    val classMirror = ru.runtimeMirror(getClass.getClassLoader)
    val classTest = classMirror.reflect(a)
    val fieldX = ru.typeOf[Good].decl(ru.TermName(fieldName)).asTerm
    classTest.reflectField(fieldX).set(strVal)
  }


  def main(args: Array[String]): Unit = {
    val progHeaderMetaData = new ProgramHeaderMetaData32
    val filePath = "src/main/resources/libxml2.so.2.9.1"
    val byteArray = Files.readAllBytes(Paths.get(filePath))
    val elfFile = new ElfFile(filePath)
    elfFile.printSummary()
    progHeader(elfFile.fileHeader.phOffSet, progHeaderMetaData, byteArray, EndianessEnum.LITTLE)
  }

  object CaseClassBeautifier {
    private def getCaseAccessors[T: ru.TypeTag] = ru.typeOf[T].members.collect { case m: ru.MethodSymbol if m.isCaseAccessor => m }
      .toList

    def nice[T: ru.TypeTag](x: T)(implicit classTag: ClassTag[T]): String = {
      val instance = x.asInstanceOf[T]
      val mirror = ru.runtimeMirror(instance.getClass.getClassLoader)
      val accessors = getCaseAccessors[T]
      var res = List.empty[String]
      accessors.foreach { z ⇒
        val instanceMirror = mirror.reflect(instance)
        val fieldMirror = instanceMirror.reflectField(z.asTerm)
        val s = s"${z.name} = ${fieldMirror.get}"
        res = s :: res
      }
      val beautified = x.getClass.getSimpleName + "(" + res.mkString(", ") + ")"
      beautified
    }
  }

  def progHeader(phOffSet: Int,
                 progHeaderMetaData: ProgramHeaderMetaData,
                 byteArray: Array[Byte],
                 endian: EndianessENum
                ): ProgramHeader = {

    val progHeader = new ProgramHeader()
    val classMirror = ru.runtimeMirror(getClass.getClassLoader)
    val classTest = classMirror.reflect(progHeaderMetaData)
    val typeOfProgHeaderMetaData = ru.typeOf[ProgramHeaderMetaData]
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
          val intVal = ParseUtils.asInt(byteArray, phOffSet, y.asInstanceOf[OffSetSizePair], endian)
          val fieldName = x.asMethod.name.toString
          println(y.asInstanceOf[OffSetSizePair])
          println(s"$fieldName intValue: ${intVal.toHexString}")
          val fieldTermSymbol = ru.typeOf[ProgramHeader].decl(ru.TermName(fieldName))
          if (fieldTermSymbol.isTerm) {
            val fieldTerm = fieldTermSymbol.asTerm
            val resultType = fieldTerm.info.resultType
            val progClass = classMirror.reflect(progHeader)

            if (resultType =:= ru.typeOf[Int]) {
              progClass.reflectField(fieldTerm).set(intVal)
            } else if (resultType <:< ru.typeOf[Enumeration#Value]) {
              val enumPkgPath = resultType.toString.split("""\.""").dropRight(1).mkString(".")
              println(enumPkgPath)
              val module = classMirror.staticModule(enumPkgPath)
              val obj = classMirror.reflectModule(module)
              val enumVal = obj.instance.asInstanceOf[Enumeration].apply(intVal)
              progClass.reflectField(fieldTerm).set(enumVal)
            } else {
              throw new IllegalStateException(s"Don't know how to parse the resultType: ${resultType.toString}")
            }
          }
        }
      )
    println(CaseClassBeautifier.nice(progHeader))
    progHeader
  }

}



