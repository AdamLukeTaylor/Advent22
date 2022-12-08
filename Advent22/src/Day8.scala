import scala.io.Source

object Day8 {
  val example = "data/day8_example.txt"
  val myData = "data/day8_main.txt"

  object AllDone extends Throwable

  case class Copse(input: Seq[String]) {
    def trees: Array[Array[Tree]] = input.map { line => line.toCharArray.map(hi => Tree(hi.toInt - '0'.toInt, false)) }.toArray
  }

  object Copse {
    def scanDown(copse: Array[Array[Tree]]): Array[Array[Tree]] = {
      var out: Array[Array[Tree]] = new Array[Array[Tree]](copse.length)
      for (a <- copse.indices) {
        out(a) = new Array[Tree](copse.head.length)
      }

      for (b <- copse.head.indices) {
        var highest = 0
        for (a <- copse.indices) {
          if (a == 0 || a == copse.length - 1) {
            out(a)(b) = Tree(copse(a)(b).height, true)
          }
          else if (highest < copse(a)(b).height) {
            out(a)(b) = Tree(copse(a)(b).height, true)
          }
          else {
            out(a)(b) = Tree(copse(a)(b).height, copse(a)(b).isVisible)
          }
          highest = Math.max(highest, copse(a)(b).height)
        }
      }
      out
    }

    def scanUp(copse: Array[Array[Tree]]): Array[Array[Tree]] = {
      var out: Array[Array[Tree]] = new Array[Array[Tree]](copse.length)
      for (a <- copse.indices) {
        out(a) = new Array[Tree](copse.head.length)
      }

      for (b <- copse.head.indices) {
        var highest = 0
        for (a <- copse.indices.reverse) {
          if (a == 0 || a == copse.length - 1) {
            out(a)(b) = Tree(copse(a)(b).height, true)
          }
          else if (highest < copse(a)(b).height) {
            out(a)(b) = Tree(copse(a)(b).height, true)
          }
          else {
            out(a)(b) = Tree(copse(a)(b).height, copse(a)(b).isVisible)
          }
          highest = Math.max(highest, copse(a)(b).height)
        }
      }
      out
    }

    def scanLeft(copse: Array[Array[Tree]]): Array[Array[Tree]] = {
      var out: Array[Array[Tree]] = new Array[Array[Tree]](copse.length)
      for (a <- copse.indices) {
        out(a) = new Array[Tree](copse.head.length)
        var highest = 0
        for (b <- copse.head.indices) {
          if (b == 0 || b == copse.head.length - 1) {
            out(a)(b) = Tree(copse(a)(b).height, true)
          }
          else if (highest < copse(a)(b).height) {
            out(a)(b) = Tree(copse(a)(b).height, true)
          }
          else {
            out(a)(b) = Tree(copse(a)(b).height, copse(a)(b).isVisible)
          }
          highest = Math.max(highest, copse(a)(b).height)
        }
      }
      out
    }

    def scanRight(copse: Array[Array[Tree]]): Array[Array[Tree]] = {
      var out: Array[Array[Tree]] = new Array[Array[Tree]](copse.length)
      for (a <- copse.indices) {
        out(a) = new Array[Tree](copse.head.length)
        var highest = 0
        for (b <- copse.head.indices.reverse) {
          if (b == 0 || b == copse.head.length - 1) {
            out(a)(b) = Tree(copse(a)(b).height, true)
          }
          else if (highest < copse(a)(b).height) {
            out(a)(b) = Tree(copse(a)(b).height, true)
          }
          else {
            out(a)(b) = Tree(copse(a)(b).height, copse(a)(b).isVisible)
          }
          highest = Math.max(highest, copse(a)(b).height)
        }
      }
      out
    }

    def viewingScores(copse: Array[Array[Tree]]): Array[Array[Int]] = {
      var out: Array[Array[Int]] = new Array[Array[Int]](copse.length)
      for (a <- copse.indices) {
        out(a) = new Array[Int](copse.head.length)
        for (b <- copse.head.indices) {
          val me = copse(a)(b).height
          // look left
          var leftCount = 0
          try {
            for (left <- 1 to b) {
              if (copse(a)(b - left).height < me) {
                leftCount = leftCount + 1
              }
              else throw AllDone
            }
          }
          catch {
            case AllDone => leftCount=leftCount+1
          }
          // look right
          var rightCount = 0
          try {
            for (right <- b + 1 until  copse.head.length) {
              if (copse(a)(right).height < me) {
                rightCount = rightCount + 1
              }
              else throw AllDone
            }
          }
          catch {
            case AllDone => rightCount=rightCount+1
          }
          // look up
          var upCount = 0
          try {
            for (up <- 1 to a) {
              if (copse(a - up)(b).height < me) {
                upCount = upCount + 1
              }
              else throw AllDone
            }
          }
          catch {
            case AllDone => upCount=upCount+1
          }
          // look up
          var downCount = 0
          try {
            for (down <- a + 1 until copse.length) {
              if (copse( down)(b).height < me) {
                downCount = downCount + 1
              }
              else throw AllDone
            }
          }
          catch {
            case AllDone =>downCount=downCount+1
          }
          //println(s"$a $b $upCount $downCount $leftCount $rightCount")
          out(a)(b) = upCount*downCount*leftCount*rightCount//if(!copse(a)(b).isVisible)upCount*downCount*leftCount*rightCount else 0
        }
      }
      out
    }
  }

  case class Tree(height: Int, isVisible: Boolean)

  type Path = List[String]

  def firstPart(exampleFile: Boolean) {
    val dataFile = if (exampleFile) example else myData

    /////
    val input = Source.fromFile(dataFile).getLines.toList
    val trees = Copse.scanDown(Copse.scanUp(Copse.scanRight(Copse.scanLeft(Copse(input).trees))))
    println(trees.toSeq.map(_.toSeq.count(_.isVisible)).sum)
    //println(trees.toSeq.map(_.toSeq.map(item=> if(item.isVisible)item.height.toString else "x").mkString).mkString("\n"))
  }

  def secondPart(exampleFile: Boolean) {
    val dataFile = if (exampleFile) example else myData

    /////
    val input = Source.fromFile(dataFile).getLines.toList
    val trees = Copse.viewingScores(Copse.scanDown(Copse.scanUp(Copse.scanRight(Copse.scanLeft(Copse(input).trees)))))
    println(trees.toSeq.map(_.toSeq.max).max)
  }
}
