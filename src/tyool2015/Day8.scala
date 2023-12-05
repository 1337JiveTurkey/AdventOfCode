package tyool2015

object Day8 extends Main {
	def main(args: Array[String]): Unit = {
		star1()
	}

	def star1(): Unit = {
		val lines = fileLines("Day8.txt")
		println(lines.map(line => line.length - parsedLength(line)).sum)
	}

	def star2(): Unit = {
		val lines = fileLines("Day8.txt")
		println(lines.map(escapedLength).sum)
	}

	def parsedLength(str: String): Int = {
		val withoutQuotes = str.substring(1, str.length - 1)
		var length = 0
		var inSlash = false
		for (c <- withoutQuotes) {
			length += 1
			if (inSlash) {
				inSlash = false
				c match {
					case 'x' => length -= 3
					case _ => length -= 1
				}
			}
			else if (c == '\\')
				inSlash = true
		}
		length
	}

	def escapedLength(str: String): Int = {
		???
	}
}
