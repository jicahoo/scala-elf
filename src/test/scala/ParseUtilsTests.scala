import FileHeader.EndianessEnum
import org.scalatest.{BeforeAndAfter, FunSuite}

class ParseUtilsTests extends FunSuite with BeforeAndAfter {

  var byteArray: Array[Byte] = _

  before {
    byteArray = List(0xAB, 0x10, 0x00, 0x20).map(_.toByte).toArray
  }

  test("Parse 1 byte correctly") {
    val intVal = ParseUtils.asInt(byteArray, 1, 2, EndianessEnum.BIG)
    assert(intVal === (1<<12))
  }

  test ("More test") (pending)

}

