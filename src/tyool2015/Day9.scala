package tyool2015

import scala.collection.mutable
import scala.util.matching.Regex

object Day9 extends Main {
	val linePattern: Regex = """(\w+) to (\w+) = (\d+)""".r

	def main(args: Array[String]): Unit = {
		star2()
	}

	def star1(): Unit = {
		val lines = fileLines("Day9.txt")

		val distances = new mutable.HashMap[(String, String), Int]
		val names = new mutable.HashSet[String]
		for (linePattern(from, to, distance) <- lines) {
			names.add(from)
			names.add(to)
			distances.put((from, to), distance.toInt)
			distances.put((to, from), distance.toInt)
		}

		var minimumDistance = Int.MaxValue
		for (permutation <- names.toList.permutations) {
			val distance = pairwise(permutation.iterator).map(distances.getOrElse(_, 0)).sum
			if (distance < minimumDistance) {
				minimumDistance = distance
			}
		}
		println(minimumDistance)
	}

	def star2(): Unit = {
		val lines = fileLines("Day9.txt")

		val distances = new mutable.HashMap[(String, String), Int]
		val names = new mutable.HashSet[String]
		for (linePattern(from, to, distance) <- lines) {
			names.add(from)
			names.add(to)
			distances.put((from, to), distance.toInt)
			distances.put((to, from), distance.toInt)
		}

		var maximumDistance = Int.MinValue
		for (permutation <- names.toList.permutations) {
			val distance = pairwise(permutation.iterator).map(distances.getOrElse(_, 0)).sum
			if (distance > maximumDistance) {
				maximumDistance = distance
			}
		}
		println(maximumDistance)
	}
}
