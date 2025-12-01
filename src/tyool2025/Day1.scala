package tyool2025

import scala.util.matching.Regex

object Day1 extends Main {
	val lineExpr: Regex = """([LR])(\d+)""".r

	def main(args: Array[String]): Unit = {
		star1()
	}

	def star1(): Unit = {
		val lines = fileLines("Day1.txt")
		val distances = for (lineExpr(lr, amount) <- lines) yield {
			val direction = if (lr == "L") -1 else 1
			amount.toInt * direction
		}
		var at = 50
		var count = 0
		for (distance <- distances) {
			at = (((at + distance) % 100) + 100) % 100
			if (at == 0) {
				count += 1
			}
		}
		println(count)
	}
}
