package tyool2022

object Day1 extends Main {
	def main(args: Array[String]): Unit = {
		star2()
	}

	def star1(): Unit = {
		val lines = fileLines("Day1.txt")
		val stanzas = splitOnBlanks(lines)
		val sumsBuilder = IndexedSeq.newBuilder[Int]
		for (stanza <- stanzas) {
			// Turn the stanza's lines into integers, then sum them up
			sumsBuilder.addOne(stanza.map(_.toInt).sum)
		}
		val sums = sumsBuilder.result()

		println(sums.max)
	}

	def star2(): Unit = {
		val lines = fileLines("Day1.txt")
		val stanzas = splitOnBlanks(lines)
		val sumsBuilder = IndexedSeq.newBuilder[Int]
		for (stanza <- stanzas) {
			// Turn the stanza's lines into integers, then sum them up
			sumsBuilder.addOne(stanza.map(_.toInt).sum)
		}
		val sums = sumsBuilder.result()

		println(sums.sorted.reverse.take(3).sum)
	}
}
