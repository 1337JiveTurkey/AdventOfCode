package tyool2022

object Day3 extends Main {
	def main(args: Array[String]): Unit = {
		star2()
	}

	def star1(): Unit = {
		val lines = fileLines("Day3.txt")
		var sum = 0
		for (line <- lines) {
			val (foo, bar) = splitLine(line)
			val first = Set.from(foo)
			val second = Set.from(bar)
			val intersection = first.intersect(second)
			sum += priority(intersection.head)
		}
		println(sum)
	}

	def star2(): Unit = {
		val lines = fileLines("Day3.txt")
		var sum = 0
		for (lineGroup <- lines.grouped(3)) {
			val first = Set.from(lineGroup.head)
			val second = Set.from(lineGroup.tail.head)
			val third = Set.from(lineGroup.tail.tail.head)
			val intersection = first intersect second intersect third
			sum += priority(intersection.head)
		}
		println(sum)
	}

	def splitLine(line: String): (String, String) = {
		(line.substring(0, line.length / 2), line.substring(line.length / 2))
	}

	def priority(char: Char): Int = {
		if (char.isUpper)
			char.toLower.toInt - 96 + 26
		else
			char.toInt - 96
	}
}
