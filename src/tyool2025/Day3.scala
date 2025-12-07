package tyool2025

object Day3 extends Main {
	def main(args: Array[String]): Unit = {
		star2()
	}

	def star1(): Unit = {
		val lines = fileLines("Day3.txt")
		val maxs = for (line <- lines) yield {
			val powerBank = line.map(toDigit)
			greedyPower(powerBank, 1)
//			val totals = for { i <- 0 until powerBank.length - 1
//			                   j <- i + 1 until powerBank.length } yield {
//				powerBank(i) * 10 + powerBank(j)
//			}
//			totals.max
		}
		println(maxs.sum)
	}

	def star2(): Unit = {
		val lines = fileLines("Day3.txt")
		val maxs = for (line <- lines) yield {
			val powerBank = line.map(toDigit)
			greedyPower(powerBank)
		}
		println(maxs.sum)
	}

	def greedyPower(powerBank: IndexedSeq[Int], digitsAfterThis: Int = 11, soFar: Long = 0): Long = {
		// The last battery we can touch in the array
		val lastBattery = powerBank.length - digitsAfterThis
		var maxBatterySoFar = powerBank(0)
		var maxBatteryPosition = 0
		for (i <- 1 until lastBattery) {
			val battery = powerBank(i)
			if (battery > maxBatterySoFar) {
				maxBatterySoFar = battery
				maxBatteryPosition = i
			}
		}
		if (digitsAfterThis > 0) {
			greedyPower(powerBank.drop(maxBatteryPosition + 1), digitsAfterThis - 1, soFar * 10 + maxBatterySoFar)
		} else {
			soFar * 10 + maxBatterySoFar
		}

	}
}
