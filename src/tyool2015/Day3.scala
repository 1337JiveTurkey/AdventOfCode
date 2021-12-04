package tyool2015

import scala.collection.mutable

object Day3 extends Main {
	def main(args: Array[String]): Unit = {
		star5()
		star6()
	}


	case class Loc(x: Int, y: Int) {
		def nextLoc(c: Char): Loc = {
			c match {
				case '^' => Loc(x, y + 1)
				case 'v' => Loc(x, y - 1)
				case '>' => Loc(x + 1, y)
				case '<' => Loc(x - 1, y)
			}
		}
	}

	def star5(): Unit = {
		val line = fileLine("Day3.txt")
		val locSet = new mutable.HashSet[Loc]()
		var s = Loc(0, 0)
		for (c <- line) {
			s = s.nextLoc(c)
			locSet.add(s)
		}
		println(locSet.size)
	}

	def star6(): Unit = {
		val line = fileLine("Day3.txt")
		val (santa, robosanta) = halves(line)

		val locSet = new mutable.HashSet[Loc]()
		var s = Loc(0, 0)
		var rs = Loc(0, 0)
		locSet.add(s)
		locSet.add(rs)
		for (c <- santa) {
			s = s.nextLoc(c)
			locSet.add(s)
		}
		for (c <- robosanta) {
			rs = rs.nextLoc(c)
			locSet.add(rs)
		}
		println(locSet.size)
	}
}
