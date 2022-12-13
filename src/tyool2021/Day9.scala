package tyool2021

import common.{DirectionSet, Grid}

object Day9 extends Main {
	def main(args: Array[String]): Unit = {
		star1()
	}

	def star1(): Unit = {
		val lines = fileLines("Day9.txt")
		val grid: Grid[Int] = Grid(lines)(toDigit)
		for (cell <- grid.cells) {
			for {direction <- DirectionSet.Cardinals
			     Some(otherCell) = cell.get(direction)} {

			}
		}
	}
}
