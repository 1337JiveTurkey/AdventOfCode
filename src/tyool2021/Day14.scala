package tyool2021

import scala.collection.SortedMap
import scala.util.matching.Regex

object Day14 extends Main {
	val template = """VCOPVNKPFOOVPVSBKCOF"""
	val arrowLine: Regex = """([A-Z])([A-Z]) -> ([A-Z])""".r

	// Head transforms include the first character. All others get their first
	// character from the previous transform so we don't include that.
	val (headTransforms, tailTransforms) = {
		val retVal1 = SortedMap.newBuilder[String, String]
		val retVal2 = SortedMap.newBuilder[String, String]
		for (line <- fileLines("Day14.txt")) {
			line match {
				case arrowLine(x,y,z) =>
					retVal1.addOne(x + y, x + z + y)
					retVal2.addOne(x + y, z + y)
			}
		}
		(retVal1.result(), retVal2.result())
	}

	def main(args: Array[String]): Unit = {
		star1()
	}

	def star1(): Unit = {
		val iterator = Iterator.iterate(template)(transform)
		for (i <- 1 to 15) {
			println(s"$i: " + countCharacters(iterator.next()))
		}
	}

	private def transform(string: String) = {
		// Analogous
		val tail = pairwiseStrings(string)
		val head = tail.next()
		val sb = new StringBuilder
		sb.append(headTransforms.getOrElse(head, head))
		for (pair <- tail) {
			val transform = tailTransforms.getOrElse(pair, pair.substring(1))
			sb.append(transform)
		}
		sb.result()
	}
}
