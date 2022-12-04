import scala.io.Source

case class Jobs(input: String){
  def first: Seq[Int] = {
    val range = input.split(",").head.split("-").map(thing => thing.toInt).toSeq
    (range.head to range.takeRight(1).head).toList
  }
  def second: Seq[Int] = {
    val range = input.split(",").tail.head.split("-").map(thing => thing.toInt).toSeq
    (range.head to range.takeRight(1).head).toList
  }
  def overlaps: Boolean = first.containsSlice(second) || second.containsSlice(first)
  def bigIntersect: Boolean = first.intersect(second) .nonEmpty
}
object Day4 {
  val example = "data/day4_example.txt"
  val myData = "data/day4_main.txt"

  def firstPart(exampleFile: Boolean) {
    val dataFile = if (exampleFile) example else myData


    /////
    val input = Source.fromFile(dataFile).getLines.toList
    val jobs = input.map(thing => Jobs(thing))
    println(jobs.map(_.second))
    println(jobs.map(_.overlaps))
    println(jobs.map(_.overlaps).count(_==true))
   }

  def secondPart(exampleFile: Boolean) {
    val dataFile = if (exampleFile) example else myData


    /////
    val input = Source.fromFile(dataFile).getLines.toList
    val jobs = input.map(thing => Jobs(thing))
    println(jobs.map(_.second))
    println(jobs.map(_.bigIntersect))
    println(jobs.map(_.bigIntersect).count(_ == true))
     }

}
