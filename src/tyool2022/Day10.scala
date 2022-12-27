package tyool2022

import scala.util.matching.Regex

object Day10 extends Main {
	def main(args: Array[String]): Unit = {
		star2()
	}

	private val AddxPattern: Regex = """addx (-?\d+)""".r
	private val NoopPattern: Regex = """noop""".r

	private def genSequence(lines: IndexedSeq[String]): IndexedSeq[Int] = {
		val outputBuilder = IndexedSeq.newBuilder[Int]
		var xRegister = 1
		outputBuilder.addOne(xRegister)
		for (line <- lines) line match {
			case AddxPattern(amount) =>
				val toAdd = amount.toInt
				// Delay Slot
				outputBuilder.addOne(xRegister)
				xRegister += toAdd
				outputBuilder.addOne(xRegister)
			case NoopPattern() => outputBuilder.addOne(xRegister)
		}
		outputBuilder.result()
	}

	def star1(): Unit = {
		val lines = fileLines("Day10.txt")
		val output: IndexedSeq[Int] = genSequence(lines)
		var sum = 0
		for (i <- 20 to 220 by 40) {
			sum += i * output(i)
			println(i * output(i))
		}
		println(sum)
	}

	def star2(): Unit = {
		val lines = fileLines("Day10.txt")
		val output: IndexedSeq[Int] = genSequence(lines)
		for (y <- 0 to 5) {
			for(x <- 0 to 40) {
				val spriteCenter = output(40 * y + x)
				val sprite = (spriteCenter - 1) to (spriteCenter + 1)
				if (sprite.contains(x)) {
					print("#")
				} else {
					print(" ")
				}
			}
			println()
		}
	}
}
