package tyool2018

import scala.collection.mutable

object Day5 extends Main {
	def main(args: Array[String]): Unit = {
		star2()
	}

	def star1(): Unit = {
		var line = fileLine("Day5.txt")
		var lineLength = line.length
		do {
			lineLength = line.length
			line = shrinkString(line)
			println(lineLength)
		} while (lineLength != line.length)
	}

	def star2(): Unit = {
		val originalLine = fileLine("Day5.txt")
		for (char <- 'a' to 'z') {
			var line = removeLetter(originalLine, char)
			var lineLength = line.length
			do {
				lineLength = line.length
				line = shrinkString(line)
			} while (lineLength != line.length)
			println(char + ": " + lineLength)
		}
	}

	def removeLetter(line: String, remove: Char): String = {
		val sb = new mutable.StringBuilder()
		for (char <- line) {
			if (char.toLower != remove) {
				sb.append(char)
			}
		}
		sb.toString()
	}

	def shrinkString(line: String): String = {
		val sb = new mutable.StringBuilder()
		var lastMatched = false
		var lastSecond: Char = '\u0000'
		for ((first, second) <- pairwise(line.iterator)) {
			lastSecond = second
			if (lastMatched) {
				lastMatched = false
			} else {
				if (crossCaseMatch(first, second)) {
					lastMatched = true
				} else {
					sb.append(first)
				}
			}
		}
		sb.append(lastSecond)
		sb.toString()
	}

	/**
	 *
	 * @param first First character to match
	 * @param second Second character to match
	 * @return True if the characters are identical but different case
	 */
	def crossCaseMatch(first: Char, second: Char): Boolean = {
		if (first.isUpper && second.isLower || first.isLower && second.isUpper) {
			first.toLower == second.toLower
		} else
			false
	}
}
