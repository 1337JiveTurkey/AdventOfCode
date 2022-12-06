package tyool2022

object Day6 extends Main {
	def main(args: Array[String]): Unit = {
		star2()
	}

	def star1(): Unit = {
		val line = fileLine("Day6.txt")
		var i = 0
		for (possibleMarker <- line.sliding(4)) {
			if (countCharacters(possibleMarker).size == 4) {
				println(i + 4)
			}
			i += 1
		}
	}

	def star2(): Unit = {
		val line = fileLine("Day6.txt")
		var i = 0
		for (possibleMarker <- line.sliding(14)) {
			if (countCharacters(possibleMarker).size == 14) {
				println(i + 14)
			}
			i += 1
		}
	}
}
