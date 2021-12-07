package tyool2021

object Day7 extends Main {
	def main(args: Array[String]): Unit = {
		star13()
	}

	def star13(): Unit = {
		val positions = fileNumbers("Star13.txt")
		for (i <- 0 to 3000) {
			val total = totalDelta(i, positions)
			println(s"$i\t$total")
		}
	}

	def totalDelta(position: Int, positions: IndexedSeq[Int]): Int = {
		positions.map((x: Int) => Math.abs(position - x)).sum
	}
}
