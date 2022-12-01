import scala.io.Source

object Day1 {
  def firstPart(exampleFile: Boolean) {
    val example = "data/day1_example.txt"
    val myData = "data/day1_main.txt"
    val dataFile = if (exampleFile) example else myData
    val input = Source.fromFile(dataFile).getLines.toList
    var output: Seq[Int] = Seq.empty
    var total = 0
    input.foreach { item =>
      if (item.toIntOption.isDefined) total += item.toInt else {
        output = output ++ Seq(total)
        total = 0
      }
    }
    println(s"${input}")
    println(s"${output}")
    println(s"${output.max}")
  }
  def secondPart(exampleFile: Boolean) {
    val example = "data/day1_example.txt"
    val myData = "data/day1_main.txt"
    val dataFile = if (exampleFile) example else myData
    val input = Source.fromFile(dataFile).getLines.toList
    var output: Seq[Int] = Seq.empty
    var total = 0
    input.foreach { item =>
      if (item.toIntOption.isDefined) total += item.toInt else {
        output = output ++ Seq(total)
        total = 0
      }
    }
    println(s"${input}")
    println(s"${output}")
    println(s"${output.sorted.reverse.take(3)}")
    println(s"${output.sorted.reverse.take(3).sum}")
  }

}
