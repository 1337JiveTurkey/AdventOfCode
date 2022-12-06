package tyool2019

object Day2 extends Main {
	def main(args: Array[String]): Unit = {
		star3()
	}

	def star3(): Unit = {
		val intcode = new Intcode(fileLine("Day2.txt"))
		intcode.mem(1) = 12
		intcode.mem(2) = 2
		for (line <- intcode) {
			println(line)
		}
		println(intcode.mem(0))
	}

	def star4(): Unit = {
		for (i <- 0 to 99) {
			for (j <- 0 to 99) {
				val intcode = new Intcode(fileLine("Day2.txt"))
				intcode.mem(1) = i
				intcode.mem(2) = j
				for (line <- intcode) {
					// println(line)
					// Run silently
				}
				val result = intcode.mem(0)
				if (result == 19690720) {
					println(s"$result found at $i$j")
				}
			}
		}
	}
}
