package tyool2021

import scala.collection.mutable
import scala.util.matching.Regex

object Day6 extends Main {

	def main(args: Array[String]): Unit = {
		star11()
	}

	val digit: Regex = """\d""".r

	def star11(): Unit = {
		val line = fileLine("Star11.txt")
		val fish = new Array[Long](9)
		for (i <- digit.findAllIn(line)) {
			fish(i.toInt) += 1
		}
		println(fish)
		for (i <- 0 to 256) {
			println("Day " + i + ": " + fish.sum)
			val spawned = fish(0)
			fish(0) = fish(1)
			fish(1) = fish(2)
			fish(2) = fish(3)
			fish(3) = fish(4)
			fish(4) = fish(5)
			fish(5) = fish(6)
			fish(6) = fish(7) + spawned
			fish(7) = fish(8)
			fish(8) = spawned
		}
	}
}
