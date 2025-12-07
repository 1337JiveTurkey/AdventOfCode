package tyool2025

import grid._

object Day4 extends Main {
	def main(args: Array[String]): Unit = {
		star1()
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
}
