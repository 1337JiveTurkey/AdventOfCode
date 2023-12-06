package tyool2023

import scala.util.matching.Regex

object Day6 extends Main {
	def main(args: Array[String]): Unit = {
		star2()
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

	def star2(): Unit = {
		val lines = fileLines("Day6.txt")
		val timesLine = lines(0)
		val recordsLine = lines(1)

		val time = numberPattern.findAllIn(timesLine).fold("")(_ + _)
		val record = numberPattern.findAllIn(recordsLine).fold("")(_ + _)
		val race = Race(time.toLong, record.toLong)

		println(race.numberOfWinners)
	}

	case class Race(time: Long, record: Long) {
		def distance(x: Long): Long = (time - x) * x
		def winner(x: Long): Boolean = distance(x) > record

		def numberOfWinners: Int = Range.Long(1, time, 1).count(winner)
	}
}
