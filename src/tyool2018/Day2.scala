package tyool2018

object Day2 extends Main {
	def main(args: Array[String]): Unit = {
		star2()
	}

	def star1(): Unit = {
		val lines = fileLines("Day2.txt")
		var twoChars = 0
		var threeChars = 0
		for (line <- lines) {
			val characters = countCharacters(line)
			if (characters.values.exists(_ == 2)) {
				twoChars += 1
			}
			if (characters.values.exists(_ == 3)) {
				threeChars += 1
			}
		}
		println(twoChars * threeChars)
	}

	def star2(): Unit = {
		val lines = fileLines("Day2.txt")
		for ((x, y) <- allPairs(lines) if hammingDistance(x, y) == 1) {
			println(x)
			println(y)
		}
	}

	def hammingDistance(str1: String, str2: String): Int = {
		assert(str1.length == str2.length)
		var diffs = 0
		for (i <- str1.indices) {
			if (str1(i) != str2(i)) {
				diffs += 1
			}
		}
		diffs
	}
}
