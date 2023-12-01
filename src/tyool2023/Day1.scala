package tyool2023

import scala.util.matching.Regex

object Day1 extends Main {
	val firstDigit: Regex = """(?<=^\D*)\d""".r
	val lastDigit: Regex = """\d(?=\D*$)""".r
	val textDigits: Regex = """(one|two|three|four|five|six|seven|eight|nine)""".r

	def main(args: Array[String]): Unit = {
		star2()
	}

	def star1(): Unit = {
		val lines = fileLines("Day1.txt")

		println(lines.map(getCalibrationValue).sum)
	}

	def star2(): Unit = {
		val lines = fileLines("Day1.txt")

		println(lines.map(textToDigits).map(getCalibrationValue).sum)
	}

	private def getCalibrationValue(str: String): Int = {
		val number = firstDigitInString(str) + lastDigitInString(str)
		number.toInt
	}

	private def firstDigitInString(str: String): String = {
		firstDigit.findFirstIn(str).get
	}

	private def lastDigitInString(str: String): String = {
		lastDigit.findFirstIn(str).get
	}

	private def textToDigits(str: String): String = {
		replacement(replacement(str))
	}

	private def replacement(str: String): String = {
		textDigits.replaceAllIn(str, digit => digit.group(0) match {
			case "one" => "1e"
			case "two" => "2o"
			case "three" => "3e"
			case "four" => "4r"
			case "five" => "5e"
			case "six" => "6x"
			case "seven" => "7n"
			case "eight" => "8t"
			case "nine" => "9e"
		})
	}

}
