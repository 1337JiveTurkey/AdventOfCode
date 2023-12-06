package tyool2023

import scala.util.matching.Regex

object Day6 extends Main {
	def main(args: Array[String]): Unit = {
		star1()
	}

	val numberPattern: Regex = """\d+""".r

	def star1(): Unit = {
		val lines = fileLines("Day6.txt")
		val timesLine = lines(0)
		val recordsLine = lines(1)
		val pairs = numberPattern.findAllIn(timesLine).zip(numberPattern.findAllIn(recordsLine))
		val races = for ((time, record) <- pairs)
			yield Race(time.toInt, record.toInt)

		for (race <- races) {
			println(race.numberOfWinners)
		}
	}

	case class Race(time: Int, record: Int) {
		def distance(x: Int): Int = (time - x) * x
		def winner(x: Int): Boolean = distance(x) > record

		def numberOfWinners: Int = (1 until time).count(winner)
	}
}
