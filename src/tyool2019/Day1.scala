package tyool2019

object Day1 extends Main {
	def main(args: Array[String]): Unit = {
		star2()
	}

	def star1(): Unit = {
		var lines = fileLines("Day1.txt")
		var numbers = lines.flatMap(_.toIntOption)
		println(numbers.map(_ / 3 - 2).sum)
	}

	def star2(): Unit = {
		var lines = fileLines("Day1.txt")
		var numbers = lines.flatMap(_.toIntOption)
		println(numbers.map(fuelNeeded).sum)
	}

	def fuelNeeded(mass: Int): Int = {
		val fuel = mass / 3 - 2
		if (fuel <= 0) {
			0
		} else {
			fuel + fuelNeeded(fuel)
		}
	}
}
