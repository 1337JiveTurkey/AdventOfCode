package tyool2016

import scala.collection.mutable
import scala.util.matching.Regex

object Day1 extends Main {
	def main(args: Array[String]): Unit = {
		star2()
	}
	val step: Regex = "([LR])([0-9]+)".r
	def star1(): Unit = {
		val line = fileLine("Day1.txt")
		val values = line.split(", ")
		val start: Direction = North
		var dir = start
		val pairs = values.map {
			case step(turn, distance) => {
				if (turn == "L")
					dir = dir.left
				else
					dir = dir.right
				(dir, distance.toInt)
			}
		}
		var x: Int = 0
		var y: Int = 0
		for (pair <- pairs) {
			x = x + (pair._1.x * pair._2)
			y = y + (pair._1.y * pair._2)
		}
		println(s"$x, $y")
	}

	def star2(): Unit = {
		val line = fileLine("Day1.txt")
		val values = line.split(", ")
		val start: Direction = North
		var dir = start
		val pairs = values.map {
			case step(turn, distance) => {
				if (turn == "L")
					dir = dir.left
				else
					dir = dir.right
				(dir, distance.toInt)
			}
		}
		val visited = new mutable.HashSet[(Int, Int)]
		var x: Int = 0
		var y: Int = 0
		for ((direction, distance) <- pairs) {
			for (d <- 1 to distance) {
				x = x + direction.x
				y = y + direction.y
				val foo = (x, y)
				if (visited.contains(foo)) {
					visited.add((x, y))
					println(s"$direction $distance -> $foo (Duplicate)")
				} else {
					println(s"$direction $distance -> $foo")
					visited.add((x, y))
				}
			}
		}
	}

	trait Direction {
		def x: Int
		def y: Int
		def left: Direction
		def right: Direction
	}

	case object North extends Direction {
		val x: Int = 0
		val y: Int = 1
		val left: Direction = West
		val right: Direction = East
	}
	case object East extends Direction {
		val x: Int = 1
		val y: Int = 0
		val left: Direction = North
		val right: Direction = South
	}
	case object South extends Direction {
		val x: Int = 0
		val y: Int = -1
		val left: Direction = East
		val right: Direction = West
	}
	case object West extends Direction {
		val x: Int = -1
		val y: Int = 0
		val left: Direction = South
		val right: Direction = North
	}

}
