package tyool2021

object Day3 extends Main {
	def main(args: Array[String]): Unit = {
		star5()
	}

	def star5(): Unit = {
		val lines = fileLines("Day3.txt")
		val bits = 12

		var gamma = 0
		var epsilon = 0

		// The value of the bit in the position we start at (2^11)
		var toAdd = 1 << (bits - 1)

		for (i <- 0 until bits) {
			val ones = lines count(_(i) == '1')
			val zeroes = lines count(_(i) == '0')
			if (ones > zeroes) {
				gamma += toAdd
			}
			if (zeroes > ones) {
				epsilon += toAdd
			}
			toAdd >>= 1
		}
		println(gamma)
		println(epsilon)
		println(gamma * epsilon)
	}

	def star6(): Unit = {
		val lines = fileLines("Day3.txt")
		val bits = 12

		var nextOxy = lines
		var nextCo2 = lines

		for (i <- 0 until bits) {
			var oxyOnes = nextOxy.filter(_(i) == '1')
			var oxyZeros = nextOxy.filter(_(i) == '0')
			nextOxy = if (nextOxy.length == 1)
				nextOxy
			else if (oxyOnes.length >= oxyZeros.length)
				oxyOnes
			else
				oxyZeros
			var co2Ones = nextCo2.filter(_(i) == '1')
			var co2Zeros = nextCo2.filter(_(i) == '0')
			nextCo2 = if (nextCo2.length == 1)
				nextCo2
			else if (co2Ones.length >= co2Zeros.length)
				co2Zeros
			else
				co2Ones
		}
		val oxy = Integer.parseUnsignedInt(nextOxy.head, 2)
		val co2 = Integer.parseUnsignedInt(nextCo2.head, 2)

		println(oxy)
		println(co2)
		println(oxy * co2)
	}
}
