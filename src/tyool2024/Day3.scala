package tyool2024

import scala.util.matching.Regex

object Day3 extends Main {
	val mulExpr: Regex = """mul\((\d{1,3}),(\d{1,3})\)""".r
	// Finds regions that start with a do() or the start of the string and end
	// with a don't() or the end of the string. The text in between is captured
	// by the only capturing group.
	val doExpr: Regex = """(?:^|do\(\))(.+?)(?:$|don't\(\))""".r

	def main(args: Array[String]): Unit = {
		star2()
	}

	def star1(): Unit = {
		val lines = fileLines("Day3.txt")
		val lineSums = for (line <- lines) yield {
			val products = for (exprMatch <- mulExpr.findAllMatchIn(line)) yield {
				val num1 = exprMatch.group(1)
				val num2 = exprMatch.group(2)
				num1.toInt * num2.toInt
			}
			products.sum
		}
		println(lineSums.sum)
	}

	def star2(): Unit = {
		val text = fileText("Day3.txt")
		val regions = for (regionMatch <- doExpr.findAllMatchIn(text)) yield {
			regionMatch.group(1)
		}
		val regionSums = for (region <- regions) yield {
			val products = for (exprMatch <- mulExpr.findAllMatchIn(region)) yield {
				val num1 = exprMatch.group(1)
				val num2 = exprMatch.group(2)
				num1.toInt * num2.toInt
			}
			products.sum
		}
		println(regionSums.sum)
	}
}
