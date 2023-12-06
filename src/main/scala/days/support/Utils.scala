package days.support

import scala.io.Source
import scala.util.Using

object Utils {
  def readFile(fileName: String): Seq[String] = Using(Source.fromFile(fileName)) { _.getLines().toSeq }.get
}
