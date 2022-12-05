import scala.collection.mutable
import scala.io.Source

case class Parser(input:String) {
  def quantity: Int = {
    input.split(" ")(1).toInt
  }
  def source: Int = {
    input.split(" ")(3).toInt -1
  }
  def target: Int = {
    input.split(" ")(5).toInt -1
  }
}
object Day5 {
  val example = "data/day5_example.txt"
  val exampleIns = "data/day5_example_ins.txt"
  val myData = "data/day5_main.txt"
  val myDataIns = "data/day5_main_ins.txt"

  def firstPart(exampleFile: Boolean) {
    val dataFile = if (exampleFile) example else myData
    val dataInsFile = if (exampleFile) exampleIns else myDataIns
    var stacks: Seq[mutable.Stack[Char]] = Seq.empty
    for (a <- 0 until 9) stacks = stacks.appended(mutable.Stack[Char]())

    /////
    val input = Source.fromFile(dataFile).getLines.toList
    input.map { line =>
      line.zipWithIndex.map { case (letter, index) =>
        if (letter.isLetterOrDigit) {
          val stack = index / 4
          stacks(stack).push(letter)
        }
      }

    }
    stacks = stacks.map(_.reverse)
    println(stacks)

    val inputIns = Source.fromFile(dataInsFile).getLines.toList
    inputIns.foreach{ ins =>
      println(s"Ins $ins")
      val parser = Parser(ins)
      for(a <- 0  until  parser.quantity){
        println(s"parse $parser")
         stacks(parser.target).push(stacks(parser.source).pop())
        println(stacks)
      }

    }
    println(stacks.map(_.head).mkString)
  }

  def secondPart(exampleFile: Boolean) {
    val dataFile = if (exampleFile) example else myData

    ////
    val input = Source.fromFile(dataFile).getLines.toList
  }
}
