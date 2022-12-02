import scala.io.Source

case class Round(input: String) {
  def them: Guess = Round.getTheirGuess(input)

  def me: Guess = Round.getDesiredResultPlay(input)

  def score: Int = (them, me) match {
    case (Rock(), Rock()) => 3 + me.getPoints
    case (Rock(), Paper()) => 6 + me.getPoints
    case (Rock(), Scissors()) => 0 + me.getPoints
    case (Paper(), Rock()) => 0 + me.getPoints
    case (Paper(), Paper()) => 3+ me.getPoints
    case (Paper(), Scissors()) => 6 + me.getPoints
    case (Scissors(), Rock()) => 6 + me.getPoints
    case (Scissors(), Paper()) => 0+ me.getPoints
    case (Scissors(), Scissors()) => 3 + me.getPoints
  }
}

object Round {
  def getTheirGuess(in: String): Guess = in.split(" ").map(_.trim).toList.head.charAt(0) match {
    case 'A' => Rock()
    case 'B' => Paper()
    case 'C' => Scissors()
  }

  def getMyGuess(in: String): Guess = in.split(" ").map(_.trim).toList.reverse.head.charAt(0) match {
    case 'X' => Rock()
    case 'Y' => Paper()
    case 'Z' => Scissors()
  }

  def getDesiredResultPlay(in: String): Guess = in.split(" ").map(_.trim).toList.reverse.head.charAt(0) match {
    case 'X' => getTheirGuess(in) match {
      case Rock() => Scissors()
      case Paper() => Rock()
      case Scissors() => Paper()
    }
    case 'Y' => getTheirGuess(in) match {
      case Rock() => Rock()
      case Paper() => Paper()
      case Scissors() => Scissors()
    }
    case 'Z' => getTheirGuess(in) match {
      case Rock() => Paper()
      case Paper() => Scissors()
      case Scissors() => Rock()
    }
  }
}

trait Guess {
  def getPoints: Int
}

case class Rock() extends Guess {
  override def getPoints: Int = 1
}

case class Paper() extends Guess {
  override def getPoints: Int = 2
}

case class Scissors() extends Guess {
  override def getPoints: Int = 3
}

object Day2 {
  def firstPart(exampleFile: Boolean) {
    val example = "data/day2_example.txt"
    val myData = "data/day2_main.txt"
    val dataFile = if (exampleFile) example else myData


    /////
    val input = Source.fromFile(dataFile).getLines.toList
    val rounds = input.map { thing => Round(thing) }
    println(s"${input}")
    println(s"${rounds}")
    println(s"${rounds.map(_.score)}")
    println(s"${rounds.map(_.score).sum}")

  }

  def secondPart(exampleFile: Boolean) {
      val example = "data/day2_example.txt"
      val myData = "data/day2_main.txt"
      val dataFile = if (exampleFile) example else myData


      /////
      val input = Source.fromFile(dataFile).getLines.toList
      val rounds = input.map { thing => Round(thing) }
      println(s"${input}")
      println(s"${rounds}")
      println(s"${rounds.map(_.score)}")
      println(s"${rounds.map(_.score).sum}")
    }

}
