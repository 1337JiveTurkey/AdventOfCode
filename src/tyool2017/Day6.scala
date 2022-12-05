package tyool2017

object Day6 extends Main {
	def main(args: Array[String]): Unit = {
		star2()
	}

	def star1(): Unit = {
		val line = fileLine("Day6.txt")
		var numbers = line.split("\t").flatMap(_.toIntOption).toIndexedSeq
		var seen: Set[IndexedSeq[Int]] = Set(numbers)
		var i = 0
		do {
			i += 1
			seen = seen + numbers
			numbers = distribute(numbers)
		} while (!seen(numbers))
		println(i)
	}

	def star2(): Unit = {
		val line = fileLine("Day6.txt")
		var numbers = line.split("\t").flatMap(_.toIntOption).toIndexedSeq
		var seen: Set[IndexedSeq[Int]] = Set(numbers)
		do {
			seen = seen + numbers
			numbers = distribute(numbers)
		} while (!seen(numbers))
		var i = 0
		val loopStart = numbers
		do {
			i += 1
			numbers = distribute(numbers)
		} while (numbers != loopStart)
		println(i)
	}

	def distribute(numbers: IndexedSeq[Int]): IndexedSeq[Int] = {
		val max = numbers.max
		val index = numbers.indexOf(max)
		val length = numbers.length
		val newNumbers = Array.from(numbers)
		newNumbers(index) = 0
		for (i <- 1 to max) {
			val j = (index + i) % length
			newNumbers(j) = newNumbers(j) + 1
		}
		newNumbers.toIndexedSeq
	}
}
