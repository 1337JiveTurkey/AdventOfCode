package tyool2024

object Day2 extends Main {
	def main(args: Array[String]): Unit = {
		star2()
	}

	def star1(): Unit = {
		val lines = fileLines("Day2.txt")
		val safes = for (line <- lines) yield {
			val levels = dividedNumbers(line)
			isSafe(levels)
		}
		println(safes.count(identity))
	}

	def star2(): Unit = {
		val lines = fileLines("Day2.txt")
		val safes = for (line <- lines) yield {
			val levels = dividedNumbers(line)
			val dampenedAlternatives = seqOfSeqComplements(levels)
			dampenedAlternatives.exists(isSafe)
		}
		println(safes.count(identity))
	}

	/**
	 * Analyzes a series of levels (one line from the input) to determine whether
	 * they are safe.
	 *
	 * @param levels The levels of the reactor as a series of numbers.
	 * @return If the reactor levels are collectively safe
	 */
	private def isSafe(levels: IndexedSeq[Int]): Boolean = {
		var positives, negatives, zeroes, big = false
		for ((a, b) <- pairwise(levels.iterator)) {
			val diff = b - a
			positives |= diff > 0
			negatives |= diff < 0
			zeroes    |= diff == 0
			big       |= Math.abs(diff) > 3
		}
		(positives != negatives) && !zeroes && !big
	}
}
