package tyool2015

object Day4 extends Main {
	def main(args: Array[String]): Unit = {
		star7()
	}

	def star7(): Unit = {
		val prefix = "ckczppom"
		for (i <- 0 until Int.MaxValue ) {
			val hash = md5(prefix + i)
			if (hash.startsWith("000000")) {
				println("md5(" + prefix + i + ") = " + hash)
				return ()
			}
		}
	}
}
