package tyool2021

object Day8 extends Main {
	def main(args: Array[String]): Unit = {
		star15()
	}

	def star15(): Unit = {
		val lines = fileLines("Day8.txt")
		var total = 0
		for (line <- lines) {
			val parts = line.split("""\s\|\s""")
			val output = parts(1)
			total += output.
				split("\\s").
				map(_.length).
				count(x => x == 2 || x == 3 || x == 4 || x == 7)
		}
		println(total)
	}

/*
 * Unscrambling algorithm notes.
 * 1: 2 segments
 * 7: 3 segments
 * 4: 4 segments
 * 2, 3, 5: 5 segments
 * 0, 6, 9: 6 segments
 * 8: 7 Segments
 *
 * First get the four unique ones.
 * 9 is only 6 digit superset of 4
 * 0 is superset of 4 but not of 7
 * 6 is not a superset of either
 *
 * 3 is only 5 digit superset of 7
 * 5 is only 5 digit subset of 6
 * 2 is all that's left
 *
 */
	def star16(): Unit = {
		val lines = fileLines("Day8.txt")
		for (line <- lines) {
			val parts = line.split("""\s\|\s""")
			val input = parts(0).split("\\s").map(Digit)
			val output = parts(1).split("\\s").map(Digit)
		}
	}

	case class Digit(chars: String) {
		val length: Int = chars.length
		val segments: Set[Char] = Set.from(chars)

	}
}
