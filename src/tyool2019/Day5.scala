package tyool2019

object Day5 extends Main {
	def main(args: Array[String]): Unit = {
		star2()
	}

	def star1(): Unit = {
		val intcode = new Intcode(fileLine("Day5.txt"))
		intcode.input.enqueue(1)
		for (line <- intcode) {
			println(line)
		}
		while (intcode.output.nonEmpty) {
			println(intcode.output.dequeue())
		}
	}

	def star2(): Unit = {
		val intcode = new Intcode(fileLine("Day5.txt"))
		intcode.input.enqueue(5)
		for (line <- intcode) {
			println(line)
		}
		while (intcode.output.nonEmpty) {
			println(intcode.output.dequeue())
		}
	}
}
