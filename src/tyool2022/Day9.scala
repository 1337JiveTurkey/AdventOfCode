package tyool2022

import common.{Direction, East, Grid, North, South, West}

import scala.util.matching.Regex

object Day9 extends Main {
	def main(args: Array[String]): Unit = {
		star2()
	}

	val LinePattern: Regex = """([UDLR]) (\d+)""".r

	def star1(): Unit = {
		val lines = fileLines("Day9.txt")

		val grid = new Grid[Boolean](1000, 1000)
		val head = new Rope(500, 500 ,2)
		val tail = head.tail
		for (LinePattern(directionText, distanceText) <- lines) {
			val distance = distanceText.toInt
			val direction = directionText match {
				case "U" => North
				case "D" => South
				case "L" => West
				case "R" => East
			}
			for (i <- 1 to distance) {
				head.move(direction)
				grid(tail.x, tail.y) = true
			}
		}
		println(grid.iterator.count(identity))
	}

	def star2(): Unit = {
		val lines = fileLines("Day9.txt")

		val grid = new Grid[Boolean](1000, 1000)
		val head = new Rope(500, 500, 10)
		val tail = head.tail
		for (LinePattern(directionText, distanceText) <- lines) {
			val distance = distanceText.toInt
			val direction = directionText match {
				case "U" => North
				case "D" => South
				case "L" => West
				case "R" => East
			}
			for (i <- 1 to distance) {
				head.move(direction)
				grid(tail.x, tail.y) = true
			}
		}
		println(grid.iterator.count(identity))
	}


	class Rope(var x: Int, var y: Int, length: Int) {
		val next: Rope = {
			if (length > 1) {
				new Rope(x, y, length - 1)
			} else {
				null
			}
		}

		def tail: Rope = {
			if (next != null) {
				next.tail
			} else {
				this
			}
		}

		def move(d: Direction): Unit = {
			if (d == null) {
				return
			}
			x += d.dx
			y += d.dy

			if (next != null) {
				val dx = x - next.x
				val dy = y - next.y
				// If it's moved far enough away that we need to move
				if (Math.abs(dx) > 1 || Math.abs(dy) > 1)
				next.move(Direction.fromDelta(dx, dy))
			}
		}
	}
}
