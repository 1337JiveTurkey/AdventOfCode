package tyool2017

object Day1 extends Main {
	val line: String = fileLine("Day1.txt")

	def main(args: Array[String]): Unit = {
		star2()
	}

	def star1(): Unit = {
		val digits = (line + line.head).map(toDigit)
		val matches = pairwise(digits.iterator).filter(x => x._1 == x._2).map(_._1)
		println(matches.sum)
	}

	def star2(): Unit = {
		val digits = (line).map(toDigit)
		val firstHalf = digits.slice(0, digits.size / 2)
		val secondHalf = digits.slice(digits.size / 2, digits.size)
		val pairs = digits.zip(secondHalf ++ firstHalf)
		val matches = pairs.filter(x => x._1 == x._2).map(_._1)
		println(matches.sum)
	}
}
