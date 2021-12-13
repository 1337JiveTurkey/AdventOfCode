package tyool2021

import scala.util.matching.Regex

object Day5 extends Main {


	def main(args: Array[String]): Unit = {
		star9()
	}

	// Regex to match vertical lines (x coordinates match)
	val vRegex: Regex = """(\d+),(\d+) -> \1,(\d+)""".r
	// Ditto but horizontal
	val hRegex: Regex = """(\d+),(\d+) -> (\d+),\2""".r
	// All other lines
	val dRegex: Regex = """(\d+),(\d+) -> (\d+),(\d+)""".r

	def star9(): Unit = {
		val lines = fileLines("Day5.txt")
		val parsedLines = (lines flatMap {
			case vRegex(x, y1, y2) => Some(VLine(x.toInt, Math.min(y1.toInt, y2.toInt) to Math.max(y1.toInt, y2.toInt)))
			case hRegex(x1, y, x2) => Some(HLine(Math.min(x1.toInt, x2.toInt) to Math.max(x1.toInt, x2.toInt), y.toInt))
			case dRegex(x1, y1, x2, y2) => Some(DLine(x1.toInt, y1.toInt, x2.toInt, y2.toInt))
			case _ => None
		}).toIndexedSeq
		val counts = parsedLines map {
			case l: Line => l.count
			case _ => 0
		}
		println(counts.sum)
		genMap(parsedLines)
	}

	def genMap(lines: IndexedSeq[Line]): Unit = {
		val map = new Array[Int](1000000)
		for (line <- lines) {
			line match {
				case HLine(xs, y) => for (x <- xs) {
					map(x + 1000 * y) += 1
				}
				case VLine(x, ys) => for (y <- ys) {
					map(x + 1000 * y) += 1
				}
				case d: DLine => {
					for ((x, y) <- d.range) {
						map(x + 1000 * y) += 1
					}
				}
			}
		}
		println(map.count(_ >= 2))
	}

	// Doesn't generalize nicely to diagonals
	def genOverlaps(lines: IndexedSeq[Line]): Set[Position] = {
		val pBuilder = Set.newBuilder[Position]
		var overlaps = 0
		for (i <- lines.indices) {
			for (j <- i + 1 until lines.length) {
				(lines(i), lines(j)) match {
					case (a: HLine, b: HLine) =>
						if (a.y == b.y) {
							val overlap = a.x intersect b.x
							overlaps += overlap.length
							println("Overlap between " + a + " and " + b + " of length " + overlap.length)
							for (x <- overlap) {
								pBuilder += Position(x, a.y)
							}
						}
					case (a: VLine, b: VLine) =>
						if (a.x == b.x) {
							val overlap = a.y intersect b.y
							overlaps += overlap.length
							println("Overlap between " + a + " and " + b + " of length " + overlap.length)
							for (y <- overlap) {
								pBuilder += Position(a.x, y)
							}
						}
					case (a: HLine, b: VLine) =>
						if ((a.x contains b.x) && (b.y contains a.y))
							pBuilder += Position(b.x, a.y)
					case (a: VLine, b: HLine) =>
						if ((b.x contains a.x) && (a.y contains b.y))
							pBuilder += Position(a.x, b.y)
				}
			}
		}
		val retVal = pBuilder.result()
		println(overlaps)
		println(retVal.size)
		retVal
	}

	case class Position(x: Int, y: Int)

	sealed trait Line {
		def count: Int
	}
	case class HLine(x: Range, y: Int) extends Line {
		val count: Int = x.length
	}
	case class VLine(x: Int, y: Range) extends Line {
		val count: Int = y.length
	}
	case class DLine(x1: Int, y1: Int, x2: Int, y2: Int) extends Line {
		val xRange: Range = if (x1 < x2) Range.inclusive(x1, x2) else Range.inclusive(x1, x2, -1)
		val yRange: Range = if (y1 < y2) Range.inclusive(y1, y2) else Range.inclusive(y1, y2, -1)
		assert(xRange.length == yRange.length)
		val range: IndexedSeq[(Int, Int)] = xRange zip yRange
		val count: Int = range.length
	}
}
