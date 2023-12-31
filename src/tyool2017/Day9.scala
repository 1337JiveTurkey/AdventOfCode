package tyool2017

object Day9 extends Main {
	def main(args: Array[String]): Unit = {
		star1()
	}

	def star1(): Unit = {
		def lines = fileLines("Day9.txt")
		for (line <- lines) {
			val (cleanLine, count) = cleanUpLine(line)
			println(cleanLine)
			println("Count = " + count)
			println("Score = " + scoreLine(cleanLine))
		}
	}

	def cleanUpLine(line: String): (String, Int) = {
		val sb = new StringBuilder
		var count = 0
		var inJunk = false
		var escapeNext = false
		for (c <- line) {
			if (!inJunk) {
				if (c == '<') {
					inJunk = true
				} else {
					sb.append(c)
				}
			} else { // In junk
				if (escapeNext) {
					escapeNext = false
				} else {
					if (c == '>') {
						inJunk = false
					} else if (c == '!') {
						escapeNext = true
					}
					else {
						count += 1
					}
				}
			}
		}

		(sb.result(), count)
	}

	def scoreLine(line: String): Int = {
		var acc = 0
		var depth = 0
		for (c <- line) {
			if (c == '{') {
				depth += 1
			} else if (c == '}') {
				acc += depth
				depth -= 1
			}
		}
		acc
	}
}
