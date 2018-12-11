import App.{Good, getClass}
import FileHeader.EndianessEnum.EndianessENum
import ProgramHeader.ProgHeaderTypeEnum.ProgHeaderTypeEnum

import scala.reflect.runtime.{universe => ru}


class ProgramHeader {
  var pType: ProgHeaderTypeEnum  = _
  var pVirtualAddr: Int = _
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

    fieldNames.foreach( fieldName => {
        val fieldTerm = ru.typeOf[People].decl(ru.TermName(fieldName)).asTerm
        val resultType = fieldTerm.info.resultType
        if (resultType =:= ru.typeOf[Int]) {
          println("I am Int")
          peopleClass.reflectField(fieldTerm).set(map(fieldName).toInt)
        } else if (resultType.toString.endsWith("Enum")) {
          val enumPkgPath = resultType.toString.replaceAll("""\.[A-Za-z]*Enum$""","")
          val module = classMirror.staticModule(enumPkgPath)
          val obj = classMirror.reflectModule(module)
          val enumVal = obj.instance.asInstanceOf[Enumeration].apply(1)
          peopleClass.reflectField(fieldTerm).set(enumVal)
        }
      }
    )

    println(p.age)
    println(p.pType)

  }
}



