package tyool2020

object Day1 extends Main {
	def main(args: Array[String]): Unit = {
		star2()
	}

	def star1(): Unit = {
		val lines = fileLines("Day1.txt")
		for (x <- lines.indices) {
			for (y <- (x + 1) until lines.length) {
				if (lines(x).toInt + lines(y).toInt == 2020) {
					println(lines(x).toInt * lines(y).toInt)
				}
			}
		}
	}
	def star2(): Unit = {
		val lines = fileLines("Day1.txt")
		for (x <- lines.indices) {
			for (y <- (x + 1) until lines.length) {
				for (z <- (y + 1) until lines.length) {
					if (lines(x).toInt + lines(y).toInt + lines(z).toInt == 2020) {
						println(lines(x).toInt * lines(y).toInt * lines(z).toInt)
					}
				}
			}
		}
	}
}
