package tyool2021

object Day8 extends Main {
	def main(args: Array[String]): Unit = {
		star16()
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

	def star16(): Unit = {
		val lines = fileLines("Day8prime.txt")
		var total = 0
		for (line <- lines) {
			val parts = line.split("""\s\|\s""")
			val input = parts(0).split("\\s").map(Digit)
			val output = parts(1).split("\\s").map(Digit)

			matchDigits(input, output)
			organizeDigits(input)

			var remaining = Set.from(input)
			var deduced: Set[Digit] = Set.empty
			while (remaining.nonEmpty) {
				val remainingBuilder = Set.newBuilder[Digit]
				for (digit <- remaining) {
					if (!digit.attemptDeduction()) {
						remainingBuilder.addOne(digit)
					}
					else {
						deduced = deduced + digit
					}
				}
				remaining = remainingBuilder.result()
			}
			println(toNumber(output))
			total = total + toNumber(output)
		}
		println(total)
	}

	private def matchDigits(input: Array[Digit], output: Array[Digit]): Unit = {
		for (i <- output.indices) {
			val outDigit = output(i)
			for (digit <- input) {
				if (digit.segments == outDigit.segments) {
					output(i) = digit
				}
			}
		}
	}

	/**
	 *
	 * @param input The digits to form into supersets and subsets of each other
	 */
	private def organizeDigits(input: Array[Digit]): Unit = {
		for (digit <- input) {
			val supersets = Set.newBuilder[Digit]
			val subsets = Set.newBuilder[Digit]
			for (other <- input) {
				if (digit.segments.subsetOf(other.segments) && digit != other) {
					supersets.addOne(other)
				}
				if (other.segments.subsetOf(digit.segments) && digit != other) {
					subsets.addOne(other)
				}
			}
			digit.supersets = supersets.result()
			digit.subsets = subsets.result()
		}
	}

	private def toNumber(output: Array[Digit]): Int = {
		output(0).value * 1000 + output(1).value * 100 + output(2).value * 10 + output(3).value
	}

	case class Digit(chars: String) {
		var value: Int = -1
		def deduced: Boolean = value != -1
		val length: Int = chars.length
		val segments: Set[Char] = Set.from(chars)

		var supersets: Set[Digit] = Set.empty
		var subsets: Set[Digit] = Set.empty

		def supersetsContain(i: Int): Boolean = {
			for (digit <- supersets) {
				if (digit.value == i) {
					return true
				}
			}
			false
		}

		def subsetsContain(i: Int): Boolean = {
			for (digit <- subsets) {
				if (digit.value == i) {
					return true
				}
			}
			false
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
		 * 9 is only 6 segment superset of 4
		 * 0 is superset of 7 but not of 4
		 * 6 is not a superset of either
		 *
		 * 3 is only 5 digit superset of 7
		 * 5 is only 5 digit subset of 6
		 * 2 is all that's left
		 *
		 */
		def attemptDeduction(): Boolean = {
			if (length == 2) {
				value = 1
			} else if (length == 4) {
				value = 4
			} else if (length == 3) {
				value = 7
			} else if (length == 7) {
				value = 8
			} else if (length == 6) {
				if (subsetsContain(4)) {
					value = 9
				}
				else if (subsetsContain(7)) {
					value = 0
				}
				else if (!subsetsContain(7) && !subsetsContain(4)) {
					value = 6
				}
			} else if (length == 5) {
				if (subsetsContain(7)) {
					value = 3
				} else if (supersetsContain(6)) {
					value = 5
				} else if (!subsetsContain(7) && !supersetsContain(6)) {
					value = 2
				}
			}

			deduced
		}
	}
}
