package tyool2018

import grid.{Grid, Point}

import scala.util.matching.Regex

object Day6 extends Main {
	def main(args: Array[String]): Unit = {
		star2()
	}

	val LinePattern: Regex = """(\d+), (\d+)""".r

	def star1(): Unit = {
		val lines = fileLines("Day6.txt")
		val regionsBuilder = Set.newBuilder[Region]
		for (LinePattern(x, y) <- lines) {
			regionsBuilder.addOne(Region(x.toInt, y.toInt))
		}
		val regions = regionsBuilder.result()
		val grid = new Grid[Region](500, 500)

		for (cell <- grid.cells) {
			var closestDistance = Int.MaxValue
			var closestRegion: Region = null
			for (region <- regions) {
				val distance = cell.manhattanTo(region)
				if (distance < closestDistance) {
					closestRegion = region
					closestDistance = distance
				}
				else if (distance == closestDistance) {
					closestRegion = null
				}
			}
			cell.value = closestRegion
			if (closestRegion != null) {
				cell.value.size += 1
				if (cell.onEdge) {
					closestRegion.isInfinite = true
				}
			}
		}

		for (region <- regions) {
			if (!region.isInfinite) {
				println(region.size)
			}
		}
	}

	def star2(): Unit = {
		val lines = fileLines("Day6.txt")
		val regionsBuilder = List.newBuilder[Region]
		for (LinePattern(x, y) <- lines) {
			regionsBuilder.addOne(Region(x.toInt, y.toInt))
		}
		val regions = regionsBuilder.result()
		println(regions)
		val grid = new Grid[Int](500, 500)

		for (cell <- grid.cells) {
			val totalDistance = regions.map(cell manhattanTo _).sum
			cell.value = totalDistance
		}
		println(grid.cells.count(_.value < 10000))
	}

		case class Region(x: Int, y: Int) extends Point {
		var isInfinite = false
		var size = 0
	}
}
