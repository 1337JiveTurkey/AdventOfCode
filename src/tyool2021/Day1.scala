package tyool2021

object Day1 extends Main {
	def main(args: Array[String]): Unit = {
		star2()
	}

	def star2(): Unit = {
		val lines = fileLines("Star1.txt")
		val ints = lines.flatMap(_.toIntOption)
		val triples = ints.sliding(3)
		val sums = triples.map(_.sum)
		// Use ints instead for the first problem
		val pairs = pairwise(sums)
		// God knows why I need to pattern match here
		print(pairs count {
			case (x, y) => x < y
		})
	}

}