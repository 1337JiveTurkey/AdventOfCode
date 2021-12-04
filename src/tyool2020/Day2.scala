package tyool2020

import scala.util.matching.Regex

object Day2 extends Main {
	def main(args: Array[String]): Unit = {
		star4()
	}

	val lineRegex: Regex = """(\d+)-(\d+) (\w): (\w+)""".r

	def star3(): Unit = {
		val lines = fileLines("Day2.txt")
		val records = lines map {
			case lineRegex(lo, hi, char, password) => Record(lo.toInt, hi.toInt, char.head, password)
		}

		println(records.count(_.totalCriterion()))
	}

	def star4(): Unit = {
		val lines = fileLines("Day2.txt")
		val records = lines map {
			case lineRegex(lo, hi, char, password) => Record(lo.toInt, hi.toInt, char.head, password)
		}

		println(records.count(_.substringCriterion()))
	}
}

case class Record(lo: Int, hi: Int, char: Char, password: String) {
	def totalCriterion(): Boolean = {
		val charCount = password.count(_ == char)
		charCount >= lo && charCount <= hi
	}

	def substringCriterion(): Boolean = {
		(password(lo - 1) == char) != (password(hi - 1) == char)
	}
}