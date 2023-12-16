package tyool2023

object Day15 extends Main {
	def main(args: Array[String]): Unit = {
		star1()
	}

	def star1(): Unit = {
		val line = fileLine("Day15.txt")
		val parts = line.split(',')
		println(parts.map(hash).sum)
	}

	def hash(in: String): Int = {
		var acc = 0
		for (c <- in) {
			acc += c
			acc *= 17
			acc %= 256
		}
		acc
	}
}
