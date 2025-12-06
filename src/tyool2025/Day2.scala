package tyool2025

import scala.collection.immutable.NumericRange

object Day2 extends Main {
	def main(args: Array[String]): Unit = {
		star2()
	}

	def star1(): Unit = {
		val line = fileLine("Day2.txt")
		val ranges = getRanges(line)
		var totalCount: Long = 0
		for (range <- ranges) {
			for (idNumber <- range) {
				if (testInvalid(idNumber)) {
					totalCount += idNumber
				}
			}
		}
		println(totalCount)
	}

	def star2(): Unit = {
		val line = fileLine("Day2.txt")
		val ranges = getRanges(line)
		var totalCount: Long = 0
		for (range <- ranges) {
			for (idNumber <- range) {
				if (testInvalidExtended(idNumber)) {
					totalCount += idNumber
				}
			}
		}
		println(totalCount)
	}

	def testInvalid(idNumber: Long): Boolean = {
		val idString = idNumber.toString
		if (idString.length % 2 == 1) {
			false
		} else {
			val firstHalf = idString.substring(0, idString.length / 2)
			val secondHalf = idString.substring(idString.length / 2)
			firstHalf == secondHalf
		}
	}

	def testInvalidExtended(idNumber: Long): Boolean = {
		val idString = idNumber.toString
		val l = idString.length
		for (i <- 1 to l / 2) {
			if (l % i == 0) {
				val distinctSubstrings = idString.grouped(i).distinct
				if (distinctSubstrings.length == 1) {
					return true
				}
			}
		}
		false
	}

	def getRanges(line: String): Array[NumericRange.Inclusive[Long]] = {
		val intervals = line.split(',')
		for (interval <- intervals) yield {
			val pair = interval.split('-')
			pair(0).toLong to pair(1).toLong
		}
	}
}
