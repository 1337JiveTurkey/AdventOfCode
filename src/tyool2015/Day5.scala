package tyool2015

import scala.util.matching.Regex

object Day5 extends Main {
	def main(args: Array[String]): Unit = {
		star10()
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
		repeatingPair(s) && threeCharacterCriterion(s)
	}

	def repeatingPair(s: String): Boolean = {
		for (i <- 0 until s.length - 3) {
			val w = s(i)
			val x = s(i + 1)
			for(j <- i + 2 until s.length - 1) {
				val y = s(j)
				val z = s(j + 1)
				if (w == y && x == z) {
					return true
				}
			}
		}
		false
	}

	def threeCharacterCriterion(s: String): Boolean = {
		for ((x, y, z) <- tripwise(s.iterator)) {
			if (x == z) {
				return true
			}
		}
		false
	}

	def star9(): Unit = {
		val lines = fileLines("Day5.txt")
		println(lines count nice)
	}

	def star10(): Unit = {
		val lines = fileLines("Day5.txt")
		println(lines count newnice)
	}
}
