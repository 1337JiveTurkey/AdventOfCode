package tyool2023

import scala.util.matching.Regex

object Day5 extends Main {
	val intervalLine: Regex = """(\d+) (\d+) (\d+)""".r

	def main(args: Array[String]): Unit = {
		star1()
	}

	def star1(): Unit = {
		val lines = fileLines("Day5.txt")
		val stanzas = splitOnBlanks(lines)
		// The seeds line is a stanza with one line so grab it and remove the prefix
		val seedsLine = stanzas(0).head.replace("seeds: ", "")
		// Get the seeds by splitting the seeds line and converting all the numbers to longs
		val seeds = seedsLine.split("\\D+").map(_.toLong)

		val maps = for (stanza <- stanzas.tail)
			yield new AlmanacMap(stanza)

		for (seed <- seeds) {
			var result = seed
			for (map <- maps) {
				result = map(result)
			}
			println(result)
		}
	}

	class AlmanacMap(fromText: IndexedSeq[String]) extends (Long => Long) {
		val name: String = fromText.head.replace(" map:", "")
		val intervals: IndexedSeq[Interval] = {
			for (intervalLine(rangeStart, domainStart, interval) <- fromText.tail)
				yield Interval(domainStart.toLong, rangeStart.toLong, interval.toLong)
		}
		override def apply(x: Long): Long = {
			// Find an interval which x is defined within
			val interval = intervals.find(_.isDefinedAt(x))
			if (interval.isDefined) {
				interval.get.apply(x)
			} else {
				x
			}
		}

		case class Interval(domainStart: Long, rangeStart: Long, interval: Long) extends PartialFunction[Long, Long] {
			override def isDefinedAt(x: Long): Boolean = x >= domainStart && x < domainStart + interval

			override def apply(x: Long): Long = x - domainStart + rangeStart
		}
	}
}
