package tyool2023

import common.{Grid, Point}

object Day11 extends Main {
	def main(args: Array[String]): Unit = {
		star2()
	}

	def star1(): Unit = {
		def lines = fileLines("Day11.txt")

		val galaxies: IndexedSeq[Point] = parseGalaxies(lines)
		val transformedGalaxies = transformGalaxies(galaxies, 2)
		val sum: Long = sumDistances(transformedGalaxies)
		println(sum)
	}

	def star2(): Unit = {
		def lines = fileLines("Day11.txt")

		val galaxies: IndexedSeq[Point] = parseGalaxies(lines)
		val transformedGalaxies = transformGalaxies(galaxies, 1000000)
		val sum: Long = sumDistances(transformedGalaxies)
		println(sum)
	}

	private def parseGalaxies(lines: IndexedSeq[String]) = {
		val grid = Grid(lines)(identity)
		val isb = IndexedSeq.newBuilder[Point]
		for (cell <- grid.cells) {
			if (cell.value == '#') {
				isb.addOne(cell)
			}
		}
		isb.result()
	}

	private def transformGalaxies(galaxies: IndexedSeq[Point], expansion: Int): IndexedSeq[Point] = {
		var occupiedCols = galaxies.map(_.x).toSet
		var occupiedRows = galaxies.map(_.y).toSet

		val retVal = IndexedSeq.newBuilder[Point]
		for (galaxy <- galaxies) {
			var newX = 0
			var newY = 0
			for (x <- 0 until galaxy.x) {
				if (occupiedCols(x))
					newX += 1
				else
					newX += expansion
			}
			for (y <- 0 until galaxy.y) {
				if (occupiedRows(y))
					newY += 1
				else
					newY += expansion
			}
			retVal.addOne(Point(newX, newY))
		}
		retVal.result()
	}

	private def sumDistances(galaxies: IndexedSeq[Point]) = {
		var sum = 0L
		for ((g1, g2) <- allDistinctPairs(galaxies)) {
			sum += g1 manhattanTo g2
		}
		sum
	}
}
