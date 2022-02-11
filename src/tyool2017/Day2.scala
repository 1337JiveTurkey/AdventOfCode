package tyool2017

object Day2 extends Main {
	val lines = fileLines("Day2.txt")
	def main(args: Array[String]): Unit = {
		star2()
	}

	def star1(): Unit = {
		var total = 0
		for (line <- lines) {
			val numbers = line.split("\\t").map(Integer.parseInt)
			val delta = numbers.max - numbers.min
			total += delta
		}
		println(total)
	}

	def star2(): Unit = {
		var total = 0
		for (line <- lines) {
			val numbers = line.split("\\t").map(Integer.parseInt)
			val divisible = allPairs(numbers).filter(x => x._1 % x._2 == 0).map(x => x._1 / x._2)
			total += divisible.sum
		}
		println(total)
	}
}
