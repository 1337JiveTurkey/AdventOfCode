package tyool2016

object Day5 extends Main {
	def main(args: Array[String]): Unit = {
		star1()
	}

	def star1(): Unit = {
		val prefix = "uqwqemis"
		val hashes = for (i <- (0 to Int.MaxValue).iterator) yield md5(prefix + i)
		val filtered = hashes.filter(_.startsWith("00000"))
		for (i <- filtered.take(8)) {
			println(i)
		}
	}
}
