package tyool2020

import scala.util.matching.Regex
import scala.util.matching.Regex.Groups

object Day7 extends Main {
	def main(args: Array[String]): Unit = {
		star1()
	}

	val bagLine: Regex = """(\w+ \w+) bags contain (.+)\.""".r
	val contentsPattern: Regex = """(\d+) (\w+ \w+) bag""".r

	def star1(): Unit = {
		val lines = fileLines("Day7.txt")
		val bags = parseBags(lines)
	}

	private def parseBags(lines: IndexedSeq[String]): Map[String, Bag] = {
		val bm = Map.newBuilder[String, Bag]
		for (bagLine(outerDesc, contents) <- lines) {
			val hmb = Map.newBuilder[String, Int]
			for (Groups(count, innerDesc) <- contentsPattern.findAllMatchIn(contents)) {
				hmb.addOne((innerDesc, count.toInt))
			}
			val bag = Bag(outerDesc, hmb.result())
			bm.addOne(outerDesc -> bag)
		}
		bm.result()
	}

	case class Bag(desc: String, children: Map[String, Int]) {

	}
}
