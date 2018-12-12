import java.io.File

import scala.reflect.runtime.{universe => ru}

object App {

  class Good {
    var a: String = _
    var b: String = _
    var c: String = _

    def setMyVal(v: String): Unit = {
      println("O was called")
      a = v
    }
  }

  case class Better(var a: String = null,
                    var b: String = null)

  def setVal[T](a: Any, fieldName: String, strVal: T): Unit = {
    val classMirror = ru.runtimeMirror(getClass.getClassLoader)
    val classTest = classMirror.reflect(a)
    val fieldX = ru.typeOf[Good].decl(ru.TermName(fieldName)).asTerm
    classTest.reflectField(fieldX).set(strVal)
  }

  def methodReflectDemo(): Unit = {
    val classMirror = ru.runtimeMirror(getClass.getClassLoader)
    val g = new Good
    val classTest = classMirror.reflect(g)

    //Scala style
    val methods = ru.typeOf[Good]
    val method = methods.decl(ru.TermName(s"setMyVal")).asMethod
    val result = classTest.reflectMethod(method)("zzzzzzzzzzzzzzzzzzzz")
    println(g.a)


    //Java style
    val myMethod = g.getClass.getDeclaredMethod("setMyVal", classOf[String])
    myMethod.invoke(g, "gggggggggg")
    println(g.a)
  }

  def main(args: Array[String]): Unit = {
    var pwd = new File(".").getAbsolutePath
    println(pwd)
    val filePath = "src/main/resources/libxml2.so.2.9.1"
    val elfFile = new ElfFile(filePath)
    elfFile.printSummary()


  }

}

