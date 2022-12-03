package tyool2022

object Day2 extends Main {

	def main(args: Array[String]): Unit = {
		star2()
	}

	def star1(): Unit = {
		val lines = fileLines("Day2.txt")
		val pairs = lines.map(translate)
		var sum = 0
		for ((theirs, mine) <- pairs) {
			sum += getPoints(theirs, mine)
		}
		println(sum)
	}

	def star2(): Unit = {
		val lines = fileLines("Day2.txt")
		val pairs = lines.map(translate2)
		var sum = 0
		for ((theirs, outcome) <- pairs) {
			sum += getPoints(theirs, outcome)
		}
		println(sum)
	}

	def translate(line: String): (Choice, Choice) = {
		val theirs = line.charAt(0) match {
			case 'A' => Rock
			case 'B' => Paper
			case 'C' => Scissors
		}
		val mine = line.charAt(2) match {
			case 'X' => Rock
			case 'Y' => Paper
			case 'Z' => Scissors
		}
		(theirs, mine)
	}

	def translate2(line: String): (Choice, Outcome) = {
		val theirs = line.charAt(0) match {
			case 'A' => Rock
			case 'B' => Paper
			case 'C' => Scissors
		}
		val outcome = line.charAt(2) match {
			case 'X' => Lose
			case 'Y' => Draw
			case 'Z' => Win
		}
		(theirs, outcome)
	}

	def getPoints(theirs: Choice, mine: Choice): Int = {
		mine.outcome(theirs).points + mine.points
	}

	def getPoints(theirs: Choice, outcome: Outcome): Int = {
		val mine = theirs.choose(outcome)
		getPoints(theirs, mine)
	}

	sealed trait Choice {
		val points: Int
		def outcome(other: Choice): Outcome
		def choose(outcome: Outcome): Choice
	}
	case object Rock extends Choice {
		override val points: Int = 1

		override def outcome(other: Choice): Outcome = other match {
			case Rock => Draw
			case Paper => Lose
			case Scissors => Win
		}

		override def choose(outcome: Outcome): Choice = outcome match {
			case Win  => Paper
			case Lose => Scissors
			case Draw => Rock
		}
	}
	case object Paper extends Choice {
		override val points: Int = 2

		override def outcome(other: Choice): Outcome = other match {
			case Rock => Win
			case Paper => Draw
			case Scissors => Lose
		}

		override def choose(outcome: Outcome): Choice = outcome match {
			case Win => Scissors
			case Lose => Rock
			case Draw => Paper
		}
	}
	case object Scissors extends Choice {
		override val points: Int = 3

		override def outcome(other: Choice): Outcome = other match {
			case Rock => Lose
			case Paper => Win
			case Scissors => Draw
		}

		override def choose(outcome: Outcome): Choice = outcome match {
			case Win => Rock
			case Lose => Paper
			case Draw => Scissors
		}
	}

	sealed trait Outcome {
		val points: Int
	}
	case object Win extends Outcome {
		override val points: Int = 6
	}
	case object Lose extends Outcome {
		override val points: Int = 0
	}
	case object Draw extends Outcome {
		override val points: Int = 3
	}
}
