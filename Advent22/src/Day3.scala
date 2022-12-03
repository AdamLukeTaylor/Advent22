import scala.io.Source

object Day3 {
  val example = "data/day3_example.txt"
  val myData = "data/day3_main.txt"

  case class Rucksack(input: String) {
    def firstPocket: String = input.take(input.length / 2)

    def secondPocket: String = input.takeRight(input.length / 2)

    def intersection: Char = firstPocket.toCharArray.toSeq.intersect(secondPocket.toCharArray.toSeq).head
  }

  object Rucksack {
    def charToValue(input: Char): Int = {
      val offset = if (input.isUpper) 26 else 0
      input.toLower.toInt - 'a'.toInt + 1 + offset
    }

    def bigIntersection(input: Seq[Rucksack]): Char = {
     val one = input.head.input.toCharArray.toSeq.intersect(input(1).input.toCharArray.toSeq)
     val two = input.head.input.toCharArray.toSeq.intersect(input(2).input.toCharArray.toSeq)
      one.intersect(two).head
    }
  }

  def firstPart(exampleFile: Boolean) {
    val dataFile = if (exampleFile) example else myData


    /////
    val input = Source.fromFile(dataFile).getLines.toList
    val rucksacks = input.map { thing => Rucksack(thing) }
    println(s"${input}")
    println(s"${rucksacks}")
    println(s"${rucksacks.map(_.intersection)}")
    println(s"${rucksacks.map(thing => Rucksack.charToValue(thing.intersection))}")
    println(s"${rucksacks.map(thing => Rucksack.charToValue(thing.intersection)).sum}")
  }

  def secondPart(exampleFile: Boolean) {
    val dataFile = if (exampleFile) example else myData


    /////
    val input = Source.fromFile(dataFile).getLines.toList
    val rucksacks = input.map { thing => Rucksack(thing) }
    val teams = rucksacks.grouped(3)
    val teamsBadge=teams.map(team=>Rucksack.bigIntersection(team)).toSeq
    println(s"${input}")
    println(s"${teams}")
    println(s"${teamsBadge}")
    println(s"${teamsBadge.map(Rucksack.charToValue).sum}")
  }

}
