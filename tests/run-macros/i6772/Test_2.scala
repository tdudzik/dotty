import scala.quoted.*
import Macros.*

object Test {
  def main(args: Array[String]): Unit = {
    println(m())
  }
}
