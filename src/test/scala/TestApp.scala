import sys.process._
object TestApp {
  def main(args: Array[String]): Unit = {
    val stdout = new StringBuilder
    val status = "python" ! ProcessLogger(stdout append _)
    println(status)
    println(stdout)

    val stderr = new StringBuilder
    "dir" ! ProcessLogger(stdout append _, stderr append _)
  }
}
