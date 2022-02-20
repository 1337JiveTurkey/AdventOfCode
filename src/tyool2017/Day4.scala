package tyool2017

object Day4 extends Main {
	def main(args: Array[String]): Unit = {
		star2()
	}

	def star1(): Unit = {
		val lines = fileLines("Day4.txt")
		var valid = 0
		for (line <- lines) {
			val words = line.split(" ")
			val wordSet = Set.from(words)
			if (words.size == wordSet.size) {
				valid = valid + 1
			}
		}
		println(valid)
	}

	def star2(): Unit = {
		val lines = fileLines("Day4.txt")
		var valid = 0
		for (line <- lines) {
			val words = line.split(" ").map(_.sorted)
			val wordSet = Set.from(words)
			if (words.size == wordSet.size) {
				valid = valid + 1
			}
		}
		println(valid)
	}

}
