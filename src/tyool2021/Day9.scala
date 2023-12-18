package tyool2021

import common.{Cell, DirectionSet, Grid}

object Day9 extends Main {
	def main(args: Array[String]): Unit = {
		star2()
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

	def star2(): Unit = {
		val lines = fileLines("Day9.txt")
		val grid: Grid[Int] = Grid(lines)(toDigit)
		val lowPointBuilder = List.newBuilder[Cell[Int]]
		for (cell <- grid.cells) {
			val neighbors = cell.neighbors(DirectionSet.Cardinals)
			// This is the low point if no neighbor has a cell height less than or equal to this cell
			val lowPoint = !neighbors.exists(neighbor => neighbor.value <= cell.value)
			if (lowPoint) {
				lowPointBuilder.addOne(cell)
			}
		}
		val lowPoints = lowPointBuilder.result()
		println(lowPoints.length)

		val is = IndexedSeq.newBuilder[Int]
		for (cell <- lowPoints) {
			val region = cell.floodFill(DirectionSet.Cardinals) {
				_.value != 9
			}
			println(region.length)
			is.addOne(region.length)
		}
		val sorted = is.result().sorted.reverse
		println(sorted.take(3).product)
	}
}