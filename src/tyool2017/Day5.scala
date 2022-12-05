package tyool2017

object Day5 extends Main {
	def main(args: Array[String]): Unit = {
		star2()
	}

	def star1(): Unit = {
		val lines = fileLines("Day5.txt")
		val numbers = lines.map(_.toInt).toBuffer
		var steps = 0
		var pc = 0
		while (numbers.indices.contains(pc)) {
			steps += 1
			val oldPc = pc
			pc += numbers(oldPc)
			numbers(oldPc) = numbers(oldPc) + 1
		}
		println(steps)
	}

	def star2(): Unit = {
		val lines = fileLines("Day5.txt")
		val numbers = lines.map(_.toInt).toBuffer
		var steps = 0
		var pc = 0
		while (numbers.indices.contains(pc)) {
			steps += 1
			val oldPc = pc
			val jumpLength = numbers(oldPc)
			pc += jumpLength
			if (jumpLength >= 3) {
				numbers(oldPc) = jumpLength - 1
			} else {
				numbers(oldPc) = jumpLength + 1
			}
		}
		println(steps)
	}
}
