package tyool2022

import scala.util.matching.Regex

object Day10 extends Main {
	def main(args: Array[String]): Unit = {
		star2()
	}

	val AddxPattern: Regex = """addx (-?\d+)""".r
	val NoopPattern: Regex = """noop""".r

	def star1(): Unit = {
		val lines = fileLines("Day10.txt")
		val outputBuilder = IndexedSeq.newBuilder[Int]
		var latest = 1
		outputBuilder.addOne(latest)
		outputBuilder.addOne(latest)
		for (line <- lines) line match {
			case AddxPattern(amount) => {
				val toAdd = amount.toInt
				// Delay Slot
				outputBuilder.addOne(latest)
				latest += toAdd
				outputBuilder.addOne(latest)
			}
			case NoopPattern() => outputBuilder.addOne(latest)
		}
		var sum = 0
		val output = outputBuilder.result()
		for (i <- 20 to 220 by 40) {
			sum += i * output(i)
			println(i * output(i))
		}
		println(sum)
	}

	def star2(): Unit = {
		val lines = fileLines("Day10Prime.txt")
		val outputBuilder = IndexedSeq.newBuilder[Int]
		var latest = 1
		outputBuilder.addOne(latest)
		outputBuilder.addOne(latest)
		for (line <- lines) line match {
			case AddxPattern(amount) => {
				val toAdd = amount.toInt
				// Delay Slot
				outputBuilder.addOne(latest)
				latest += toAdd
				outputBuilder.addOne(latest)
			}
			case NoopPattern() => outputBuilder.addOne(latest)
		}
		val output = outputBuilder.result()
		for (y <- 0 to 5) {
			for(x <- 0 until 40) {
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
