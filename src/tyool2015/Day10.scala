package tyool2015

object Day10 extends Main {
	val input = """1113222113"""
	def main(args: Array[String]): Unit = {
		star2()
	}

	def star1(): Unit = {
		var foo = input
		for (i <- 1 to 40) {
			foo = nextSequence(foo)
		}
		println(foo.length)
	}

	def star2(): Unit = {
		var foo = input
		for (i <- 1 to 50) {
			foo = nextSequence(foo)
		}
		println(foo.length)
	}

	def nextSequence(string: String): String = {
		val sb = new StringBuilder
		var digit = 'x'
		var count = 0
		for (c <- string) {
			if (c == digit) {
				count += 1
			} else {
				// Special case the initial 'x' and don't print it
				if (count > 0) {
					sb.append(count).append(digit)
				}
				digit = c
				count = 1
			}
		}
		if (count > 0) {
			sb.append(count).append(digit)
		}
		sb.result()
	}
}
