import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
object Day6 {
  val example = "data/day6_example.txt"
  val myData = "data/day6_main.txt"

  def firstPart(exampleFile: Boolean) {
    val dataFile = if (exampleFile) example else myData

    /////
    val input = Source.fromFile(dataFile).getLines.toList
    for ((elem, inx) <- input.head.sliding(4).zipWithIndex) {
      if(elem.toCharArray.toSeq.distinct.size==4){
        println(elem)
        println(inx + 4)
        sys.exit(0)
      }
    }

  }

  def secondPart(exampleFile: Boolean) {
    val dataFile = if (exampleFile) example else myData

    /////
    val input = Source.fromFile(dataFile).getLines.toList
    for ((elem, inx) <- input.head.sliding(14).zipWithIndex) {
      if (elem.toCharArray.toSeq.distinct.size == 14) {
        println(elem)
        println(inx + 14)
        sys.exit(0)
      }
    }

  }
}
