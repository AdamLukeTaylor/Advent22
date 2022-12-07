import Day7.build

import scala.collection.mutable
import scala.io.Source

object Day7 {
  val example = "data/day7_example.txt"
  val myData = "data/day7_main.txt"

  type Path = List[String]

  def build(input: Seq[String]): Seq[(List[String],Int)] = {
    val root = List("/")
    val initial = Map(root -> 0)
    val (_, sizes) = input.foldLeft((root, initial)) { case ((path, sizes), line) =>
      line match {
        case "$ ls" => (path, update(path, sizes, -sizes(path)))
        case "$ cd \\" => (List(path.last), sizes)
        case "$ cd .." => (path.tail, sizes)
        case s"$$ cd $name" => (name :: path, sizes.updated(name :: path, 0))
        case s"dir $name" => (path, sizes)
        case s"$size $name" => (path, update(path, sizes, size.toInt))
      }
    }
    sizes.toSeq
  }

  def update(path: Path, sizes: Map[Path, Int], delta: Int): Map[Path, Int] = path match {
    case Nil => sizes
    case _ :: tail => update(tail, sizes, delta).updated(path, sizes(path) + delta)
  }


  def firstPart(exampleFile: Boolean) {
    val dataFile = if (exampleFile) example else myData

    /////
    val input = Source.fromFile(dataFile).getLines.toList

    val sizes = build(input)
    println(sizes.map(_._2).filter(_ <= 100000).sum)


  }

  def secondPart(exampleFile: Boolean) {
    val dataFile = if (exampleFile) example else myData

    /////
    val input = Source.fromFile(dataFile).getLines.toList
    val sizes = build(input)
      val free = 70000000 - sizes.map(_._2).max

    val candidates = sizes.filter{case (path, size)=> size + free > (30000000)}
println(candidates.map(_._2).min)
  }
}
