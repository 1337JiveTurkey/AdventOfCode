package tyool2024

import common.{BearingSet, Cell, DirectionSet, Grid, Northeast, Northwest, Southeast, Southwest}

object Day4 extends Main {
	def main(args: Array[String]): Unit = {
		star2()
	}

	def star1(): Unit = {
		val lines = fileLines("Day4.txt")
		val grid = Grid(lines)(identity)
		var total = 0
		for (cell <- grid.cells) {
			for (direction <- cell.validDirections) {
				if (isXmas(cell :: cell.ray(direction))) {
					total += 1
				}
			}
		}
		println(total)
	}

	def isXmas(ray: List[Cell[Char]]): Boolean = {
		ray match {
			case Cell('X') :: Cell('M') :: Cell('A') :: Cell('S') :: _ => true
			case _ => false
		}
	}

	def star2(): Unit = {
		val lines = fileLines("Day4.txt")
		val grid = Grid(lines)(identity)
		var total = 0
		for (cell <- grid.cells) {
			if (isMasX(cell)) {
				total += 1
			}
		}
		println(total)
	}

	/**
	 *
	 * @param cell The center cell to test
	 * @return True if this has a pair of MASes in an X pattern
	 */
	def isMasX(cell: Cell[Char]): Boolean = {
		// Center must be an A not on a grid edge so values fit
		if (!cell.onEdge && cell.value == 'A') {
			val ne = cell.get(Northeast).get.value
			val nw = cell.get(Northwest).get.value
			val se = cell.get(Southeast).get.value
			val sw = cell.get(Southwest).get.value
			val pattern1 = nw == 'M' && ne == 'M' && se == 'S' && sw == 'S'
			val pattern2 = nw == 'S' && ne == 'M' && se == 'M' && sw == 'S'
			val pattern3 = nw == 'S' && ne == 'S' && se == 'M' && sw == 'M'
			val pattern4 = nw == 'M' && ne == 'S' && se == 'S' && sw == 'M'
			pattern1 || pattern2 || pattern3 || pattern4
		} else {
			false
		}
	}
}
