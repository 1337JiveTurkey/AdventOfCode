package tyool2023

import common.{Cell, Grid, Point}

object Day3 extends Main {
	def main(args: Array[String]): Unit = {
		star2()
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
				// Test if any of the digits were adjacent to a symbol
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

	private def isPeriod(cell: Cell[Char]): Boolean = cell.value == '.'

	private def isDigit(cell: Cell[Char]): Boolean = cell.value >= '0' && cell.value <= '9'

	private def isSymbol(cell: Cell[Char]): Boolean = !isPeriod(cell) && !isDigit(cell)

	def star2(): Unit = {
		val lines = fileLines("Day3.txt")
		val grid = Grid(lines)(identity)
		val numbers: List[Number] = getNumbersFromGrid(grid)
		val stars: List[Cell[Char]] = getStarsFromGrid(grid)
		var sum = 0
		for (star <- stars) {
			val partNumbers = numbers.filter(_.inRange(star))
			println(partNumbers)
			if (partNumbers.length == 2) {
				val gearRatio = partNumbers.head.value * partNumbers.tail.head.value
				sum += gearRatio
			}
		}
		println(sum)
	}

	private def getNumbersFromGrid(grid: Grid[Char]): List[Number] = {
		var currentNumber = 0
		var x = 0
		var y = 0
		val numbersBuilder = List.newBuilder[Number]
		for (cell <- grid.cells) {
			if (isDigit(cell)) {
				if (currentNumber == 0) {
					x = cell.x - 1
					y = cell.y - 1
				}
				val digit = toDigit(cell.value)
				// Increase the number by the new digit
				currentNumber = currentNumber * 10 + digit
			}
			else {
				if (currentNumber != 0) {
					val length = digits(currentNumber).length
					numbersBuilder.addOne(Number(currentNumber, x to (x + length + 1), y to (y + 2)))
				}
				currentNumber = 0
			}
		}
		numbersBuilder.result()
	}

	private def getStarsFromGrid(grid: Grid[Char]): List[Cell[Char]] = {
		val symbolsBuilder = List.newBuilder[Cell[Char]]
		for (cell <- grid.cells) {
			if (cell.value == '*') {
				symbolsBuilder.addOne(cell)
			}
		}
		symbolsBuilder.result()
	}

	/**
	 * A number on the grid, with a value and a location expressed as a pair of
	 * ranges. A symbol is adjacent if it's within both ranges.
	 *
	 * @param value The value of the number.
	 * @param xRange The x coordinates that the number spans.
	 * @param yRange The y coordinates that the number spans.
	 */
	case class Number(value: Int, xRange: Range, yRange: Range) {
		def inRange(s: Point): Boolean = xRange.contains(s.x) && yRange.contains(s.y)
	}
}
