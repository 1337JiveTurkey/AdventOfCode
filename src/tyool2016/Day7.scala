package tyool2016

import scala.collection.mutable

object Day7 extends Main {
	def main(args: Array[String]): Unit = {
		star2()
	}

	def star1(): Unit = {
		val lines = fileLines("Day7.txt")
		val addresses = lines.map(Address)
		val matches = addresses.filter(_.tlsTest())
		println(matches.length)
	}

	def star2(): Unit = {
		val lines = fileLines("Day7.txt")
		val addresses = lines.map(Address)
		val matches = addresses.filter(_.sslTest())
		println(matches.length)
	}

	case class Address(string: String) {
		val (outBrackets, inBrackets): (IndexedSeq[String], IndexedSeq[String]) = {
			val groups = string.split("""[\[\]]""")
			val ob = IndexedSeq.newBuilder[String]
			val ib = IndexedSeq.newBuilder[String]
			for ((group, i) <- groups.zipWithIndex) {
				if (i % 2 == 0) {
					ob.addOne(group)
				} else {
					ib.addOne(group)
				}
			}
			(ob.result(), ib.result())
		}

		def tlsTest(): Boolean = {
			if (inBrackets.exists(abbaTest)) {
				false
			} else {
				outBrackets.exists(abbaTest)
			}
		}

		def abbaTest(s: String): Boolean = {
			quadwise(s.iterator) exists {
				case (a, b, c, d) => a == d && b == c && a != b
			}
		}

		def sslTest(): Boolean = {
			var allBABs = Set.empty[String]
			for (outBracket <- outBrackets) {
				allBABs = allBABs union findABAs(outBracket)
			}
			var found = false
			for (inBracket <- inBrackets) {
				for (bab <- allBABs) {
					if (inBracket.contains(bab)) {
						found = true
					}
				}
			}
			found
		}

		def findABAs(s: String): Set[String] = {
			s.sliding(3).filter(isABA).map(toBAB).toSet
		}

		private def isABA(s: String): Boolean = {
			s(0) == s(2) && s(0) != s(1)
		}

		private def toBAB(aba: String): String = {
			val stringBuilder = new mutable.StringBuilder()
			stringBuilder.append(aba(1)).append(aba(0)).append(aba(1)).toString()
		}
	}
}
