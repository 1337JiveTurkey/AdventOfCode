package tyool2021

object Day7 extends Main {
	def main(args: Array[String]): Unit = {
		star13()
	}

	def star13(): Unit = {
		val positions = fileNumbers("Star13.txt")
		val sorted = positions.sorted
		println(sorted.length)
		println(sorted(499))
		println(sorted(500))
//		val min = positions.min
//		val max = positions.max
//		for (i <- min to max) {
//			val total = totalDelta(i, positions)
//			println(s"$i\t$total")
//		}
	}

	def totalDelta(position: Int, positions: IndexedSeq[Int]): Int = {
		positions.map((x: Int) => Math.abs(position - x)).sum
	}
}
