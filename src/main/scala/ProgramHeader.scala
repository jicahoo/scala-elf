import java.nio.file.{Files, Paths}

import App.Good
import FileHeader.EndianessEnum
import FileHeader.EndianessEnum.EndianessENum
import ProgramHeader.ProgHeaderTypeEnum.ProgHeaderTypeEnum

import scala.reflect.ClassTag
import scala.reflect.runtime.{universe => ru}


case class ProgramHeader(var pType: ProgHeaderTypeEnum, var pVirtualAddr: Int, var pMemSize: Int) {
  //  var pType: ProgHeaderTypeEnum  = _
  //  var pVirtualAddr: Int = _
  //  var pMemSize: Int = _
}

class People {
  var name: String = _
  var age: Int = _
  var gender: Boolean = _
  var pType: ProgHeaderTypeEnum = _
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

  def setVal[T](a: Any, fieldName: String, strVal: T): Unit = {
    val classMirror = ru.runtimeMirror(getClass.getClassLoader)
    val classTest = classMirror.reflect(a)
    val fieldX = ru.typeOf[Good].decl(ru.TermName(fieldName)).asTerm
    classTest.reflectField(fieldX).set(strVal)
  }


  def main(args: Array[String]): Unit = {

    val classMirror = ru.runtimeMirror(getClass.getClassLoader)
    val p = new People
    val map = Map("name" -> "jack", "age" -> "20", "gender" -> "1")

    val peopleClass = classMirror.reflect(p)
    //TODO: use reflection to pop the valeu to People.
    val fieldNames = List("age", "pType")

    fieldNames.foreach(fieldName => {
      val fieldTerm = ru.typeOf[People].decl(ru.TermName(fieldName)).asTerm
      val resultType = fieldTerm.info.resultType
      if (resultType =:= ru.typeOf[Int]) {
        println("I am Int")
        peopleClass.reflectField(fieldTerm).set(map(fieldName).toInt)
      } else if (resultType.toString.endsWith("Enum")) {
        val enumPkgPath = resultType.toString.replaceAll("""\.[A-Za-z]*Enum$""", "")
        val module = classMirror.staticModule(enumPkgPath)
        val obj = classMirror.reflectModule(module)
        val enumVal = obj.instance.asInstanceOf[Enumeration].apply(1)
        peopleClass.reflectField(fieldTerm).set(enumVal)
      }
    }
    )

    println(p.age)
    println(p.pType)
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

    val progHeader = new ProgramHeader(ProgHeaderTypeEnum.PT_NULL, 0, 0)
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
          val temp = ru.typeOf[ProgramHeader].decl(ru.TermName(fieldName))
          println(fieldName)
          println(temp)
          val fieldNames = ru.typeOf[ProgramHeader].decls
            .filter(x => (!x.isConstructor) && (!x.isMethod))
            .map(_.asTerm.name.toString.trim).toList
          println(s"$fieldNames : $fieldName")
          if (fieldNames.contains(fieldName)) {
            println((0 until 10).map(_ => "##").mkString)
            val fieldTerm = ru.typeOf[ProgramHeader].decl(ru.TermName(fieldName)).asTerm
            val resultType = fieldTerm.info.resultType
            val progClass = classMirror.reflect(progHeader)

            if (resultType =:= ru.typeOf[Int]) {
              progClass.reflectField(fieldTerm).set(intVal)
            } else if (resultType.toString.endsWith("Enum")) {
              println(resultType.toString)
              val enumPkgPath = resultType.toString.replaceAll("""\.[A-Za-z]*Enum$""", "")
              val module = classMirror.staticModule(enumPkgPath)
              val obj = classMirror.reflectModule(module)
              println(intVal.toHexString)
              val enumVal = obj.instance.asInstanceOf[Enumeration].apply(intVal)
              progClass.reflectField(fieldTerm).set(enumVal)
            }
          }
        }
      )
    println(CaseClassBeautifier.nice(progHeader))
    progHeader
  }

}



