package tyool2015

import scala.collection.mutable
import scala.util.matching.Regex

object Day13 extends Main {

	val guestPattern: Regex = """(\w+) would (gain|lose) (\d+) happiness units by sitting next to (\w+).""".r

	def main(args: Array[String]): Unit = {
		star1()
	}

	def star1(): Unit = {
		val lines = fileLines("Day13.txt")

		val mirth = new mutable.HashMap[(String, String), Int]
		val names = new mutable.HashSet[String]
		for (guestPattern(g1, gainLoss, amount, g2) <- lines) {
			val pair = (g1, g2)
			names.add(g1)
			names.add(g2)
			val happiness = if (gainLoss == "gain")
				amount.toInt
			else
				-amount.toInt
			mirth.put(pair, happiness)
		}

		var highestTotal = 0
		for (arrangement <- names.toIndexedSeq.permutations) {
			var total = 0
			val extraPair = (arrangement.head, arrangement.last)
			val reversedExtraPair = (arrangement.last, arrangement.head)
			total += mirth.getOrElse(extraPair, 0) + mirth.getOrElse(reversedExtraPair, 0)
			for ((first, second) <- pairwise(arrangement.iterator)) {
				total += mirth.getOrElse((first, second), 0) + mirth.getOrElse((second, first), 0)
			}
			if (total > highestTotal) {
				highestTotal = total
			}
		}
		println(highestTotal)
	}
}
