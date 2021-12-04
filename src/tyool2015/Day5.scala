package tyool2015

import scala.util.matching.Regex

object Day5 extends Main {
	def main(args: Array[String]): Unit = {
		star9()
	}

	val threeVowels: Regex = ".*?[aeiou].*?[aeiou].*?[aeiou].*".r
	val forbiddenDigraphs: Regex = "(ab|cd|pq|xy)".r

	def nice(s: String): Boolean = {
		if (!threeVowels.matches(s)) {
			return false
		}
		val pairs: Iterator[(Char, Char)] = pairwise(s.iterator)
		for ((x, y) <- pairs) {
			if (x == y) {
				return threeVowels.matches(s) && forbiddenDigraphs.findFirstIn(s).isEmpty
			}
		}
		return false
	}

	def newnice(s: String): Boolean = {
		for (c <- s) {

		}
		false
	}

	def star9(): Unit = {
		val lines = fileLines("Day5.txt")
		println(lines count nice)
	}
}
