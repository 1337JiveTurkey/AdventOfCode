package tyool2015

import scala.collection.immutable.BitSet
import scala.collection.mutable

object Day17 extends Main {
	def main(args: Array[String]): Unit = {
		star2()
	}

	def star1(): Unit = {
		val lines = fileLines("Day17.txt")
		val numbers = lines.map(_.toInt)
		println(countPossibilities(numbers, 150))
	}

	def star2(): Unit = {
		val lines = fileLines("Day17.txt")
		val numbers = lines.map(_.toInt)
		println(possibilitiesBySize(numbers, 150))
	}

	def countPossibilities(containers: IndexedSeq[Int], remaining: Int): Int = {
		// Generate bitsets that map to subsets of containers
		val bits = BitSet.fromSpecific(containers.indices)
		// Comically inefficient
		var total = 0
		for (subset <- bits.subsets()) {
			var sum = 0
			for (i <- containers.indices) {
				if (subset(i)) {
					sum += containers(i)
				}
			}
			if (sum == remaining) {
				total += 1
			}
		}
		total
	}

	def possibilitiesBySize(containers: IndexedSeq[Int], remaining: Int): mutable.TreeMap[Int, Int] = {
		// Generate bitsets that map to subsets of containers
		val bits = BitSet.fromSpecific(containers.indices)
		val totals = mutable.TreeMap.empty[Int, Int]
		// Comically inefficient
		for (subset <- bits.subsets()) {
			val size = subset.size
			var sum = 0
			for (i <- containers.indices) {
				if (subset(i)) {
					sum += containers(i)
				}
			}
			if (sum == remaining) {
				totals.put(size, totals.getOrElse(size, 0) + 1)
			}
		}
		totals
	}
}
