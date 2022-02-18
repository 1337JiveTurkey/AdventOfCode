package tyool2016

import scala.util.matching.Regex

object Day3 extends Main {
	def main(args: Array[String]): Unit = {
		star1()
	}

	val numbers: Regex = """\s+(\d+)\s+(\d+)\s+(\d+)""".r
	def star1(): Unit = {
		val lines = fileLines("Day3.txt")
		val triples = lines.map {
			case numbers(first, second, third) => List(first.toInt, second.toInt, third.toInt).sorted
		}
		println(triples.count(t => t(0) + t(1) > t(2)))
	}

}
