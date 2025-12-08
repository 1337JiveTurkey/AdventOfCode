package tyool2025

import grid._

object Day4 extends Main {
	def main(args: Array[String]): Unit = {
		star2()
	}

	def star1(): Unit = {
		val lines = fileLines("Day4.txt")
		val paperGrid = Grid(lines) {
			case '@' => true
			case '.' => false
		}
		val liftable = for (cell <- paperGrid.cells) yield {
			val filledNeighbors = cell.neighbors.count(_.value)
			cell.value && filledNeighbors < 4
		}
		println(liftable.count(identity))
	}

	def star2(): Unit = {
		val lines = fileLines("Day4.txt")
		val paperGrid = Grid(lines) {
			case '@' => true
			case '.' => false
		}
		val countAtStart = paperGrid.countCells(_.value)
		var remainingGrid = paperGrid
		var count = countAtStart
		var oldCount = count
		do {
			oldCount = count
			remainingGrid = remainingGrid.mapCells { cell =>
				val filledNeighbors = cell.neighbors.count(_.value)
				cell.value && filledNeighbors >= 4
			}
			count = remainingGrid.countCells(_.value)
		} while (oldCount != count)
		println(countAtStart - count)
	}
}
