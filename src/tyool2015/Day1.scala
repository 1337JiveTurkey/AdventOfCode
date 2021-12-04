package tyool2015

object Day1 extends Main {
	def main(args: Array[String]): Unit = {
		star2()
	}

	def star1(): Unit = {
		val line = fileLine("Star1.txt")
		println(line.count(_ == '(') - line.count(_ == ')'))
	}

	def star2(): Unit = {
		val line = fileLine("Star1.txt")
		var floor = 0
		var count = 0
		for (c <- line) {
			count += 1
			if (c == '(')
				floor += 1
			else
				floor -= 1
			if (floor == -1) {
				println(count)
			}
		}
	}
}
