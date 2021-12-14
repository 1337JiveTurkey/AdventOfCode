package tyool2020

object Day5 extends Main {
	def main(args: Array[String]): Unit = {
		star1()
	}

	private def star1(): Unit = {
		val lines = fileLines("Day5.txt")
		val binaryLines = lines map toBinary
		val numbers = binaryLines map toNumber
		val sorted = numbers.sorted
		for ((x, y) <- pairwise(sorted.iterator)) {
			val foo = if (y - x == 1) ' ' else '!'
			println(s"$foo$x")
		}
	}

	private def toBinary(seatSeq: String): String = {
		val sb = new StringBuilder
		for (c <- seatSeq) {
			if (c == 'F' || c == 'L') {
				sb.addOne('0')
			}
			if (c == 'B' || c == 'R') {
				sb.addOne('1')
			}
		}
		sb.result()
	}

	private def toNumber(binarySeq: String): Int = {
		Integer.parseInt(binarySeq, 2)
	}
}
