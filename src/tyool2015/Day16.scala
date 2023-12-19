package tyool2015

object Day16 extends Main {

	val toMatch: IndexedSeq[String] =
		"""children: 3
			|cats: 7
			|samoyeds: 2
			|pomeranians: 3
			|akitas: 0
			|vizslas: 0
			|goldfish: 5
			|trees: 3
			|cars: 2
			|perfumes: 1""".stripMargin.linesIterator.toIndexedSeq

	private val matchLine = """([a-z]+): (\d+)""".r

	private val auntLine = """Sue (\d+): ([a-z]+): (\d+), ([a-z]+): (\d+), ([a-z]+): (\d+)""".r

	val matchMap: Map[String, Int] = {
		val retVal = Map.newBuilder[String, Int]
		for (matchLine(thing, count) <- toMatch) {
			retVal.addOne(thing -> count.toInt)
		}
		retVal.result()
	}

	def main(args: Array[String]): Unit = {
		star2()
	}

	def star1(): Unit = {
		val lines = fileLines("Day16.txt")

		for (auntLine(number, t1, c1, t2, c2, t3, c3) <- lines) {
			if (matchMap(t1) == c1.toInt && matchMap(t2) == c2.toInt && matchMap(t3) == c3.toInt) {
				println(number)
			}
		}
	}

	def star2(): Unit = {
		val lines = fileLines("Day16.txt")

		for (auntLine(number, t1, c1, t2, c2, t3, c3) <- lines) {
			if (star2Compare(t1, c1.toInt) && star2Compare(t2, c2.toInt) && star2Compare(t3, c3.toInt) ) {
				println(number)
			}
		}
	}

	def star2Compare(t: String, c: Int): Boolean = {
		t match {
			case "cats" => matchMap("cats") < c
			case "trees" => matchMap("trees") < c
			case "pomeranians" => matchMap("pomeranians") > c
			case "goldfish" => matchMap("goldfish") > c
			case _ => matchMap(t) == c
		}
	}
}
