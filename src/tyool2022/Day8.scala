package tyool2022

import common.{DirectionSet, Grid, North}

object Day8 extends Main {
	def main(args: Array[String]): Unit = {
		star2()
	}

	def star1(): Unit = {
		val lines = fileLines("Day8.txt")
		val grid: Grid[Int] = Grid(lines)(toDigit)
		var count = 0
		for (cell <- grid.cells) {
			val dirs = DirectionSet.Cardinals.iterator
			// Is there a direction where if we shoot a ray it reaches the edge?
			val visible = dirs.exists(!cell.ray(_).exists(cell.value <= _.value))
			if (visible) {
				count += 1
			}
		}
		println(count)
	}

	def star2(): Unit = {
		val lines = fileLines("Day8.txt")
		val grid: Grid[Int] = Grid(lines)(toDigit)
		var maxScore = 0
		for (cell <- grid.cells) {
			var score = 1
			for (dir <- DirectionSet.Cardinals) {
				val ray = cell.ray(dir)
				val totalLength = ray.length
				val blockedLength = ray.indexWhere(cell.value <= _.value)
				// If there's nothing blocking this is equal to -1
				if (blockedLength == -1) {
					score *= totalLength
				} else {
					score *= (blockedLength + 1)
				}
			}
			if (score > maxScore) {
				maxScore = score
			}
			println(score)
		}
		println(maxScore)

	}
}
