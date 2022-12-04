package tyool2022

import scala.util.matching.Regex

object Day4 extends Main {

	def main(args: Array[String]): Unit = {
		star2()
	}

	val LinePattern: Regex = """(\d+)-(\d+),(\d+)-(\d+)""".r

	def star1(): Unit = {
		val lines = fileLines("Day4.txt")
		var total = 0
		for (LinePattern(aString, bString, cString, dString) <- lines) {
			val a = aString.toInt
			val b = bString.toInt
			val c = cString.toInt
			val d = dString.toInt
			val range1 = a to b
			val range2 = c to d
			val overlap = range1 intersect range2
			if (overlap.equals(range1) || overlap.equals(range2)) {
				total += 1
				// Full overlap
			}
		}
		println(total)
	}

	def star2(): Unit = {
		val lines = fileLines("Day4.txt")
		var total = 0
		for (LinePattern(aString, bString, cString, dString) <- lines) {
			val a = aString.toInt
			val b = bString.toInt
			val c = cString.toInt
			val d = dString.toInt
			val range1 = a to b
			val range2 = c to d
			val overlap = range1 intersect range2
			if (overlap.nonEmpty) {
				total += 1
				// Full overlap
			}
		}
		println(total)
	}
}
