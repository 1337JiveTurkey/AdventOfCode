package tyool2021

import common.Grid

object Day9 extends Main {
	def main(args: Array[String]): Unit = {
		star1()
	}

	def star1(): Unit = {
		val lines = fileLines("Day9.txt")
		val grid: Grid[Cell] = Grid(lines)(Cell)
	}

	class Cell(val level: Int) {
	}

	// Just because I think it makes the syntax more pretty
	object Cell extends (Char => Cell) {
		override def apply(c: Char): Cell = new Cell(c.toInt)
	}
}
