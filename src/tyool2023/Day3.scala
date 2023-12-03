package tyool2023

import common.{Cell, Grid}

object Day3 extends Main {
	def main(args: Array[String]): Unit = {
		star1()
	}

	def star1(): Unit = {
		var total = 0
		val lines = fileLines("Day3.txt")
		val grid = Grid(lines)(identity)
		var currentNumber = 0
		var isAdjacent = false
		for (cell <- grid.cells) {
			if (isDigit(cell)) {
				val digit = toDigit(cell.value)
				// Increase the number by the new digit
				currentNumber = currentNumber * 10 + digit
				// Check all neighbors for if they're a symbol
				val neighborIsSymbol = cell.neighbors.exists(isSymbol)
				if (neighborIsSymbol) {
					isAdjacent = true
				}
			}
			else {
				// Test if any of the digits were adjacent to a
				if (isAdjacent && currentNumber > 0) {
					total += currentNumber
					println("Found " + currentNumber)
				}
				currentNumber = 0
				isAdjacent = false
			}
		}
		println(total)
	}

	def isPeriod(cell: Cell[Char]): Boolean = cell.value == '.'

	def isDigit(cell: Cell[Char]): Boolean = cell.value >= '0' && cell.value <= '9'

	def isSymbol(cell: Cell[Char]): Boolean = !isPeriod(cell) && !isDigit(cell)
}
