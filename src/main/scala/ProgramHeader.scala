import java.nio.file.{Files, Paths}

import App.Good
import FileHeader.EndianessEnum
import FileHeader.EndianessEnum.EndianessENum
import ProgramHeader.ProgHeaderTypeEnum.ProgHeaderTypeEnum
import com.jichao.errors.InvalidEnumValue
import com.typesafe.scalalogging.Logger

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
  val logger = Logger(classOf[ProgramHeader])

  object ProgHeaderTypeEnum extends Enumeration {
    type ProgHeaderTypeEnum = Value
    val PT_NULL, PT_LOAD, PT_DYNAMIC, PT_INTERP, PT_NOTE, PT_SHLIB, PT_PHDR = Value
    val PT_LOOS:ProgHeaderTypeEnum = Value(0x60000000)
    //https://github.com/comex/cs/blob/master/elfconst.py
    val PT_GNU_EH_FRAME: ProgHeaderTypeEnum = Value(0x6474e550)
    val PT_GNU_STACK: ProgHeaderTypeEnum = Value(0x6474e551)
    val PT_GNU_RELRO: ProgHeaderTypeEnum = Value(0x6474e552)
    val PT_LOSUNW: ProgHeaderTypeEnum = Value(0x6ffffffa)
//    val PT_SUNWBSS = Value(0x6ffffffa)
    val PT_SUNWSTACK: ProgHeaderTypeEnum = Value(0x6ffffffb)
    val PT_HISUNW:ProgHeaderTypeEnum = Value(0x6fffffff)
    //HIOS = 0x6fffffff,
    val PT_LOPROC: ProgHeaderTypeEnum = Value(0x70000000)
    val PT_HIPROC:ProgHeaderTypeEnum = Value(0x7fffffff)

  }

  def parse(byteArray: Array[Byte], phOffSet: Int, phEntSize: Int, phNum: Int, endian: EndianessENum): List[ProgramHeader] = {
    (0 until phNum).map(phOffSet + _ * phEntSize).map(
      offSet => {
        logger.debug(s"========offSet: $offSet")
        val progHeaderMetaData = new ProgramHeaderMetaData32
        val targetObjType  = ru.typeOf[ProgramHeader]
        val targetObj = new ProgramHeader()
        val metaDataType = ru.typeOf[ProgramHeaderMetaData]
        progHeader(offSet, progHeaderMetaData, targetObj, byteArray, EndianessEnum.LITTLE,
          metaDataType,
          targetObjType
        )
        logger.debug(targetObj.toString)
        targetObj
      }
    ).toList
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
    val targetObjType  = ru.typeOf[ProgramHeader]
    val targetObj = new ProgramHeader()
    val metaDataType = ru.typeOf[ProgramHeaderMetaData]
    progHeader(elfFile.fileHeader.phOffSet, progHeaderMetaData, targetObj, byteArray, EndianessEnum.LITTLE,
      metaDataType,
      targetObjType
    )
    logger.debug(CaseClassBeautifier.nice(targetObj))
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
                 metaDataObj: AnyRef,
                 targetObj: AnyRef,
                 byteArray: Array[Byte],
                 endian: EndianessENum,
                 metaDataType: ru.Type,
                 targetObjType: ru.Type
                ): AnyRef = {

    val classMirror = ru.runtimeMirror(getClass.getClassLoader)
    val classTest = classMirror.reflect(metaDataObj)

    val methods = metaDataType.decls

    methods
      .filter(m => m.isMethod && !m.isConstructor)
      .foreach(
        x => {
          val y = classTest.reflectMethod(x.asMethod)()
          val intVal = ParseUtils.asInt(byteArray, phOffSet, y.asInstanceOf[OffSetSizePair], endian)
          val fieldName = x.asMethod.name.toString
          logger.debug(y.asInstanceOf[OffSetSizePair].toString)
          logger.debug(s"$fieldName intValue: 0x${intVal.toHexString}")
          val fieldTermSymbol = targetObjType.decl(ru.TermName(fieldName))
          if (fieldTermSymbol.isTerm) {
            val fieldTerm = fieldTermSymbol.asTerm
            val resultType = fieldTerm.info.resultType
            val progClass = classMirror.reflect(targetObj)

            if (resultType =:= ru.typeOf[Int]) {
              progClass.reflectField(fieldTerm).set(intVal)
            } else if (resultType <:< ru.typeOf[Enumeration#Value]) {
              val enumPkgPath = resultType.toString.split("""\.""").dropRight(1).mkString(".")
              val module = classMirror.staticModule(enumPkgPath)
              val obj = classMirror.reflectModule(module)
              try {
                val enumVal = obj.instance.asInstanceOf[Enumeration].apply(intVal)
                progClass.reflectField(fieldTerm).set(enumVal)
              } catch {
                case e: NoSuchElementException =>
                  logger.debug(s"Can't parse $intVal for enum obj $obj. Exception is $e")
                  throw InvalidEnumValue(s"Can't parse $intVal for enum obj $obj.")
              }
            } else {
              throw new IllegalStateException(s"Don't know how to parse the resultType: ${resultType.toString}")
            }
          }
        }
      )
    //logger.debug(CaseClassBeautifier.nice(targetObj)) Not worked after change targetObj type from ProgramHeader to AnyRef
    targetObj
  }

}



