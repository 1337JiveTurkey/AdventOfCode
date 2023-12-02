package tyool2019

import scala.util.matching.Regex

object Day3 extends Main {
	def main(args: Array[String]): Unit = {
		star2()
	}

	def star1(): Unit = {
		val lines = fileLines("Day3Prime.txt")
		assert(lines.length == 2)
		val (first, second) = (lines.head, lines.tail.head)

		val (firstH, firstV) = getLines(first)
		val (secondH, secondV) = getLines(second)

		var minimum = Int.MaxValue
		for (line1 <- firstH) {
			for (line2 <- secondV) {
				val intersection = line1.distanceScore(line2)
				if (intersection.isDefined && intersection.get < minimum) {
					minimum = intersection.get
					println(line1 + " and " + line2 + " at " + minimum)
				}
			}
		}
		for (line1 <- secondH) {
			for (line2 <- firstV) {
				val intersection = line1.distanceScore(line2)
				if (intersection.isDefined && intersection.get < minimum) {
					minimum = intersection.get
					println(line1 + " and " + line2 + " at " + minimum)
				}
			}
		}
		println(minimum)
	}

	def star2(): Unit = {
		val lines = fileLines("Day3.txt")
		assert(lines.length == 2)
		val (first, second) = (lines.head, lines.tail.head)

		val (firstH, firstV) = getLines(first)
		val (secondH, secondV) = getLines(second)

		var minimum = Int.MaxValue
		for (line1 <- firstH) {
			for (line2 <- secondV) {
				val intersection = line1.latencyScore(line2)
				if (intersection.isDefined && intersection.get > 0 && intersection.get < minimum) {
					minimum = intersection.get
					println(line1 + " and " + line2 + " at " + minimum)
				}
			}
		}
		for (line1 <- secondH) {
			for (line2 <- firstV) {
				val intersection = line1.latencyScore(line2)
				if (intersection.isDefined && intersection.get > 0 && intersection.get < minimum) {
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
		var latency = 0
		for (Spec(direction, distanceString) <- spec.split(",")) {
			val distance = distanceString.toInt
			direction match {
				case "U" => {
					val newY = y + distance
					vlines.addOne(VLine(x, y to newY, y, latency))
					y = newY
				}
				case "D" => {
					val newY = y - distance
					vlines.addOne(VLine(x, newY to y, y,latency))
					y = newY
				}
				case "L" => {
					val newX = x - distance
					hlines.addOne(HLine(newX to x, y, x, latency))
					x = newX
				}
				case "R" => {
					val newX = x + distance
					hlines.addOne(HLine(x to newX, y, x, latency))
					x = newX
				}
			}
			latency += distance
		}
		(hlines.result(), vlines.result())
	}

	case class HLine(x: Range, y: Int, startingX: Int, latency: Int) {
		def distanceScore(other: VLine): Option[Int] = {
			if (x.contains(other.x) && other.y.contains(y)) {
				Some(Math.abs(other.x) + Math.abs(y))
			} else {
				None
			}
		}
		def latencyScore(other: VLine): Option[Int] = {
			if (x.contains(other.x) && other.y.contains(y)) {
				val dy = Math.abs(other.startingY - y)
				val dx = Math.abs(startingX - other.x)
				Some(latency + other.latency + dx + dy)
			} else {
				None
			}
		}
	}

	case class VLine(x: Int, y: Range, startingY: Int, latency: Int) {
	}
}
