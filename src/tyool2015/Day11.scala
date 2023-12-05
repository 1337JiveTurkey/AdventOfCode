package tyool2015

object Day11 extends Main {

	val input = """cqjxjnds"""

	def main(args: Array[String]): Unit = {
		star2()
	}

	def star1(): Unit = {
		var password = input
		do {
			password = nextPass(password)
		} while (!passValid(password))
		println(password)
	}

	def star2(): Unit = {
		var password = "cqjxxyzz"
		do {
			password = nextPass(password)
		} while (!passValid(password))
		println(password)
	}

	def nextPass(oldPass: String): String = {
		val array = oldPass.toCharArray
		for (i <- array.indices.reverse) {
			if (array(i) != 'z') {
				array(i) = (array(i) + 1).asInstanceOf[Char]
				return new String(array)
			} else {
				array(i) = 'a'
			}
		}
		new String(array) + 'a'
	}

	def passValid(str: String): Boolean = {
		noForbiddenCharacters(str) && ascendingTriple(str) && twoPairs(str)
	}

	def noForbiddenCharacters(str: String): Boolean = !str.exists(c => c == 'i' || c == 'l' || c == 'o')

	def ascendingTriple(str: String): Boolean = {
		tripwise(str.iterator).exists(a => a._1 + 1 == a._2 && a._1 + 2 == a._3)
	}

	def twoPairs(str: String): Boolean = {
		val pairPattern = """([a-z])\1""".r
		pairPattern.findAllIn(str).size >= 2
	}

}
