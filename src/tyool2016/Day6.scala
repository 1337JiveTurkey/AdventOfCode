package tyool2016

object Day6 extends Main {
	def main(args: Array[String]): Unit = {
		star2()
	}

	def star1(): Unit = {
		val lines = fileLines("Day6.txt").transpose
		for (line <- lines) {
			println(mostCommon(line))
		}
	}

	def star2(): Unit = {
		val lines = fileLines("Day6.txt").transpose
		for (line <- lines) {
			println(leastCommon(line))
		}
	}

	def mostCommon(line: Seq[Char]): Char = {
		var topChar: Char = '*'
		var topScore: Int = -1
		for ((char, count) <- countCharacters(line)) {
			if (count > topScore) {
				topChar = char
				topScore = count
			}
		}
		topChar
	}

	def leastCommon(line: Seq[Char]): Char = {
		var topChar: Char = '*'
		var topScore: Int = 99999999
		for ((char, count) <- countCharacters(line)) {
			if (count < topScore) {
				topChar = char
				topScore = count
			}
		}
		topChar
	}
}
