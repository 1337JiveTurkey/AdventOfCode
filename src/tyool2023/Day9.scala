package tyool2023

object Day9 extends Main {
	def main(args: Array[String]): Unit = {
		star2()
	}

	def star1(): Unit = {
		val lines = fileLines("Day9.txt")
		val sequences = lines.map(dividedNumbers)
		var sumOfNewValues = 0
		for (sequence <- sequences) {
			val differenceStack = generateDifferenceStack(sequence)
			val lasts = differenceStack.map(_.last)
			val sumOfLasts = lasts.sum
			sumOfNewValues += sumOfLasts
		}
		println(sumOfNewValues)
	}

	def star2(): Unit = {
		val lines = fileLines("Day9.txt")
		val sequences = lines.map(dividedNumbers)
		var sumOfNewValues = 0
		for (sequence <- sequences) {
			val differenceStack = generateDifferenceStack(sequence)
			// Now we get the first of each sequence of differences
			val firsts = differenceStack.map(_.head)
			var newFirst = 0
			for(first <- firsts.reverse) {
				newFirst = first - newFirst
			}
			sumOfNewValues += newFirst
		}
		println(sumOfNewValues)
	}

	def generateDifferences(in: IndexedSeq[Int]): IndexedSeq[Int] = {
		pairwise(in.iterator).map(x => x._2 - x._1).toIndexedSeq
	}

	def allZeroes(in: IndexedSeq[Int]): Boolean = {
		!in.exists(_ != 0)
	}

	/**
	 * Generates all of the differences for a given sequence, repeating until
	 * a sequence of all zeros is found.
	 *
	 * @param in The original sequence to find the differences of.
	 * @return The repeated differences for the given sequence.
	 */
	def generateDifferenceStack(in: IndexedSeq[Int]): List[IndexedSeq[Int]] = {
		val retVal = List.newBuilder[IndexedSeq[Int]]
		retVal.addOne(in)
		var next = in
		do {
			next = generateDifferences(next)
			retVal.addOne(next)
		} while (!allZeroes(next))

		retVal.result()
	}
}
