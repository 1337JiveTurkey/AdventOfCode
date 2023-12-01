package tyool2023

import scala.util.matching.Regex

object Day1 extends Main {
	val firstDigit: Regex = """(?<=^\D*)\d""".r
	val lastDigit: Regex = """\d(?=\D*$)""".r

	def main(args: Array[String]): Unit = {
		star1()
	}

	def star1(): Unit = {
		val lines = fileLines("Day1.txt")

		println(lines.map(getCalibrationValue).sum)
	}

	def getCalibrationValue(str: String): Int = {
		val number = firstDigitInString(str) + lastDigitInString(str)
		number.toInt
	}

	def firstDigitInString(str: String): String = {
		firstDigit.findFirstIn(str).get
	}

	def lastDigitInString(str: String): String = {
		lastDigit.findFirstIn(str).get
	}

}
