import App.{Good, getClass}
import FileHeader.EndianessEnum.EndianessENum
import ProgramHeader.ProgHeaderTypeEnum.ProgHeaderTypeEnum

import scala.reflect.runtime.{universe => ru}


class ProgramHeader {
  var pType: ProgHeaderTypeEnum  = _
}


// Little goal
class People {
  var name: String = _
  var age: Int = _
  var gender: Boolean = _
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
    println("ProgramHeader")
    val a = new ProgramHeader
    val classMirror = ru.runtimeMirror(getClass.getClassLoader)
    val classTest = classMirror.reflect(a)
    val cls = ProgHeaderTypeEnum.getClass
    val cSymbol = ru.runtimeMirror(cls.getClassLoader).classSymbol(cls)
    //More clear doc: https://docs.scala-lang.org/overviews/reflection/overview.html
    //https://fair-jm.iteye.com/blog/2163746
    //https://stackoverflow.com/questions/15008367/convert-class-to-universe-type-symbol
    val classM = classMirror.reflectClass(cSymbol)
    println(classM)
    val fieldX = ru.typeOf[ProgramHeader].decl(ru.TermName("pType")).asTerm
    println(fieldX)
    println(fieldX.info.resultType.eq(ru.typeOf[ProgHeaderTypeEnum]))
    println(ru.typeOf[ProgHeaderTypeEnum])
    val x = ProgHeaderTypeEnum
    val y = ru.typeOf[ProgHeaderTypeEnum]
    val yDecls = y.decls
    println(yDecls)

    val phHeaderClass = ProgramHeader.getClass
    val pType = phHeaderClass.getDeclaredFields
    phHeaderClass.getDeclaredMethods.foreach(x => println(s"World: $x"))

    pType.foreach(x => println(s"Hello:$x"))
    phHeaderClass.getFields.foreach(x => println(s"Again: $x"))
//
    val xClass = x.getClass
    val xInst = xClass.newInstance()
    println(xInst.apply(0))

    val p = new People
    val map = Map("name" -> "jack", "age" -> "20", "gender" -> "1")

    val peopleClass = classMirror.reflect(p)
    //TODO: use reflection to pop the valeu to People.
    val fieldName = "age"

    val fieldTerm = ru.typeOf[People].decl(ru.TermName(fieldName)).asTerm
    if (fieldTerm.info.resultType =:= ru.typeOf[Int]) {
      println("I am Int")
      peopleClass.reflectField(fieldTerm).set(map(fieldName).toInt)
    }
    println(p.age)

  }
}



