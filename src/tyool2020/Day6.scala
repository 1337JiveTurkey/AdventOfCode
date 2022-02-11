package tyool2020

object Day6 extends Main {
	def main(args: Array[String]): Unit = {
		star2()
	}

	def star1(): Unit = {
		val lines = splitOnBlanks(fileLines("Day6.txt"))

		var sum = 0
		for (group <- lines) {
			var anyOf = Set[Char]()
			for (line <- group) {
				anyOf = anyOf | line.toSet
			}
			sum += anyOf.size
		}
		println(sum)
	}

	def star2(): Unit = {
		val lines = splitOnBlanks(fileLines("Day6.txt"))

		var sum = 0
		for (group <- lines) {
			var allOf = "abcdefghijklmnopqrstuvwxyz".toSet
			for (line <- group) {
				allOf = allOf & line.toSet
			}
			sum += allOf.size
		}
		println(sum)
	}
}
