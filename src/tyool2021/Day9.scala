package tyool2021

import common.{DirectionSet, Grid}

object Day9 extends Main {
	def main(args: Array[String]): Unit = {
		star1()
	}

	def star1(): Unit = {
		var total = 0
		val lines = fileLines("Day9.txt")
		val grid: Grid[Int] = Grid(lines)(toDigit)
		for (cell <- grid.cells) {
			val safety = cell.value + 1
			val neighbors = cell.neighbors(DirectionSet.Cardinals)
			// This is the low point if no neighbor has a cell height less than or equal to this cell
			val lowPoint = !neighbors.exists(neighbor => neighbor.value <= cell.value)
			if (lowPoint) {
				total += safety
			}
		}
		println(total)
	}
}
