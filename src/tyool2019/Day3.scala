package tyool2019

import scala.util.matching.Regex

object Day3 extends Main {
	def main(args: Array[String]): Unit = {
		star1()
	}

	def star1(): Unit = {
		val lines = fileLines("Day3.txt")
		assert(lines.length == 2)
		val (first, second) = (lines.head, lines.tail.head)

		val (firstH, firstV) = getLines(first)
		val (secondH, secondV) = getLines(second)

		var minimum = Int.MaxValue
		for(line1 <- firstH) {
			for (line2 <- secondV) {
				val intersection = line1.intersectScore(line2)
				if (intersection.isDefined && intersection.get < minimum) {
					minimum = intersection.get
					println(line1 + " and " + line2 + " at " + minimum)
				}
			}
		}
		for (line1 <- secondH) {
			for (line2 <- firstV) {
				val intersection = line1.intersectScore(line2)
				if (intersection.isDefined && intersection.get < minimum) {
					minimum = intersection.get
					println(line1 + " and " + line2 + " at " + minimum)
				}
			}
		}
		println(minimum)
	}

	val Spec: Regex = """([UDLR])(\d+)""".r

	def getLines(spec: String): (IndexedSeq[HLine], IndexedSeq[VLine]) = {
		val hlines = IndexedSeq.newBuilder[HLine]
		val vlines = IndexedSeq.newBuilder[VLine]

		var x = 0
		var y = 0
		for (Spec(direction, distanceString) <- spec.split(",")) {
			val distance = distanceString.toInt
			direction match {
				case "U" => {
					val newY = y + distance
					vlines.addOne(VLine(x, y to newY))
					y = newY
				}
				case "D" => {
					val newY = y - distance
					vlines.addOne(VLine(x, newY to y))
					y = newY
				}
				case "L" => {
					val newX = x - distance
					hlines.addOne(HLine(newX to x, y))
					x = newX
				}
				case "R" => {
					val newX = x + distance
					hlines.addOne(HLine(x to newX, y))
					x = newX
				}
			}
		}
		(hlines.result(), vlines.result())
	}

	case class HLine(x: Range, y: Int) {
		def intersectScore(other: VLine): Option[Int] = {
			if (x.contains(other.x) && other.y.contains(y)) {
				Some(Math.abs(other.x) + Math.abs(y))
			} else {
				None
			}
		}
	}

	case class VLine(x: Int, y: Range) {
		def intersectScore(other: HLine): Option[Int] = {
			if (y.contains(other.y) && other.x.contains(x)) {
				Some(Math.abs(other.y) + Math.abs(x))
			} else {
				None
			}
		}
	}
}
