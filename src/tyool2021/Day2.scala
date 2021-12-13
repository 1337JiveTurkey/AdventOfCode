package tyool2021

import scala.util.matching.Regex

object Day2 extends Main {
	def main(args: Array[String]): Unit = {
		star4()
	}

	// Regexes needed for parsing the file
	val forward: Regex = "forward ([0-9]+)".r
	val up: Regex = "up ([0-9]+)".r
	val down: Regex = "down ([0-9]+)".r

	// Motion in x and y axes
	case class Move(x: Int, y: Int)

	def star4(): Unit = {
		val lines = fileLines("Day2.txt")
		val parsed = lines.map {
			case forward(distance) => Move(distance.toInt, 0)
			case up(distance) => Move(0, -distance.toInt)
			case down(distance) => Move(0, distance.toInt)
			case _ => Move(0, 0) // PHP-style error handling
		}
		var aim = 0
		var horizontal = 0
		var vertical = 0
		for (m <- parsed) {
			aim += m.y
			horizontal += m.x
			vertical += m.x * aim
		}
		println("Horizontal " + horizontal)
		println("Vertical " + vertical)
		println("Product " + horizontal * vertical)
	}
}
