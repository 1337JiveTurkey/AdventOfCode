package tyool2019

object Day4 extends Main {
	def main(args: Array[String]): Unit = {
		star2()
	}

	def star1(): Unit = {
		val numbers = 307237 to 769058
		println(numbers.count(star1Criteria))
	}

	def star2(): Unit = {
		val numbers = 307237 to 769058
		println(numbers.count(star2Criteria))
	}

	def star1Criteria(number: Int): Boolean = {
		val string = number.toString
		val repeated = string.sliding(2).exists(xy => xy(0) == xy(1))
		val nonDescending = !string.sliding(2).exists(xy => xy(0) > xy(1))
		repeated && nonDescending
	}

	def star2Criteria(number: Int): Boolean = {
		val string = number.toString
		val nonDescending = !string.sliding(2).exists(xy => xy(0) > xy(1))
		val nonTriple = countCharacters(string).values.exists(_ == 2)
		nonTriple && nonDescending
	}
}
