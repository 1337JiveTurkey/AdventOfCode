package tyool2021

object Day7 extends Main {
	def main(args: Array[String]): Unit = {
		star14()
	}

	def star13(): Unit = {
		val positions = fileNumbers("Star13prime.txt")
		val sorted = positions.sorted
		val length = positions.length
		println(length)
		println(sorted(length / 2))
		println(sorted(length / 2 + 1))
		val min = positions.min
		val max = positions.max
		for (i <- min to max) {
			val total = totalDelta(i, positions)
			println(s"$i\t$total")
		}
	}

	def star14(): Unit = {
		val positions = fileNumbers("Star13.txt")
		val sorted = positions.sorted
		val length = positions.length
		val min = positions.min
		val max = positions.max
		for (i <- min to max) {
			val total = totalModifiedDelta(i, positions)
			println(s"$i\t${total.sum}")
		}
	}

	def totalDelta(position: Int, positions: IndexedSeq[Int]): Int = {
		positions.map((x: Int) => Math.abs(position - x)).sum
	}

	def modifiedDelta(position: Int, x: Int): Int = {
		val delta = Math.abs(position - x)
		delta * (delta + 1) / 2
	}

	def totalModifiedDelta(position: Int, positions: IndexedSeq[Int]): IndexedSeq[Int] = {
		positions.map((x: Int) => modifiedDelta(position, x))
	}
}
