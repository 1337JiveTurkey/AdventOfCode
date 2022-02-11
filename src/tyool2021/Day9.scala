package tyool2021

import common.Grid

object Day9 extends Main {
	def main(args: Array[String]): Unit = {
		star1()
	}

	def star1(): Unit = {
		val lines = fileLines("Day9.txt")
		val grid: Grid[Cell] = Grid.withCoordinates(lines)(Cell)
	}

	class Cell(val x: Int, val y: Int, val level: Int) {
	}

	// Just because I think it makes the syntax more pretty
	object Cell extends ((Int, Int, Char) => Cell) {
		def apply(x: Int, y: Int, c: Char): Cell = new Cell(x, y, c.toInt)
	}
}
