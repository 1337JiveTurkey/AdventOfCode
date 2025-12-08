package tyool2018

import grid.Grid

import scala.util.matching.Regex

object Day3 extends Main {

	def main(args: Array[String]): Unit = {
		star2()
	}

	val LinePattern: Regex = """#(\d+) @ (\d+),(\d+): (\d+)x(\d+)""".r

	def star1(): Unit = {
		val lines = fileLines("Day3.txt")
		val claims = new Grid[Int](1000, 1000)
		for (LinePattern(number, xString, yString, wString, hString) <- lines) {
			val x = xString.toInt
			val y = yString.toInt
			val h = hString.toInt
			val w = wString.toInt
			for (i <- x until (x + w)) {
				for (j <- y until (y + h)) {
					claims(i, j) = claims(i, j) + 1
				}
			}
		}
		var count = 0
		for (i <- claims) {
			if (i > 1) {
				count += 1
			}
		}
		println(count)
	}

	def star2(): Unit = {
		val lines = fileLines("Day3.txt")
		val claims = new Grid[Set[String]](1000, 1000)
		val allNumbers = Set.newBuilder[String]
		for (LinePattern(number, xString, yString, wString, hString) <- lines) {
			val x = xString.toInt
			val y = yString.toInt
			val h = hString.toInt
			val w = wString.toInt
			allNumbers.addOne(number)
			for (i <- x until (x + w)) {
				for (j <- y until (y + h)) {
					val claim = claims(i, j)
					if (claim == null) {
						claims(i, j) = Set(number)
					}
					else {
						claims(i, j) = claim + number
					}
				}
			}
		}
		var remaining = allNumbers.result()
		for (i <- claims) {
			if (i != null && i.size > 1) {
				remaining = remaining.removedAll(i)
			}
		}
		println(remaining)
	}
}
