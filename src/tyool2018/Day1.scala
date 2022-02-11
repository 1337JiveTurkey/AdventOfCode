package tyool2018

import scala.collection.mutable

object Day1 extends Main {

	def main(args: Array[String]): Unit = {
		star2()
	}

	def star1(): Unit = {
		val lines = fileLines("Day1.txt")
		val numbers = lines.map(Integer.parseInt)
		println(numbers.sum)
	}

	def star2(): Unit = {
		val lines = fileLines("Day1.txt")
		val numbers = lines.map(Integer.parseInt)
		val reached = new mutable.HashSet[Int]
		reached.add(0)
		var frequency = 0
		for (number <- repeatForever(numbers)) {
			frequency += number
			if (!reached.add(frequency)) {
				println(frequency)
				return
			}
		}
	}
}
