package tyool2015

import scala.util.matching.Regex

object Day2 extends Main {
	def main(args: Array[String]): Unit = {
		star4()
	}

	val dims: Regex = "([0-9]+)x([0-9]+)x([0-9]+)".r

	case class Dims(l: Int, w: Int, h: Int) {
		val listed: Seq[Int] = List(l, w, h).sorted
		val minPerimeter: Int = 2 * listed.head + 2 * listed.tail.head
		val volume: Int = l * w * h
		val face1: Int = l * w
		val face2: Int = l * h
		val face3: Int = w * h
		def paper: Int = {
			val slack = List(face1, face2, face3).min
			2 * face1 + 2 * face2 + 2 * face3 + slack
		}
		def ribbon: Int = {
			minPerimeter + volume
		}
	}

	def star3(): Unit = {
		val lines = fileLines("Day2.txt")
		val parsed = lines map {
			case dims(l, w, h) => Dims(l.toInt, w.toInt, h.toInt)
		}
		println(parsed.foldLeft(0){_ + _.paper})
	}

	def star4(): Unit = {
		val lines = fileLines("Day2.txt")
		val parsed = lines map {
			case dims(l, w, h) => Dims(l.toInt, w.toInt, h.toInt)
		}
		println(parsed.foldLeft(0){_ + _.ribbon})
	}

}
