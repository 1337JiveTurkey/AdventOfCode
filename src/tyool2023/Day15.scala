package tyool2023

import scala.collection.mutable
import scala.util.matching.Regex

object Day15 extends Main {
	def main(args: Array[String]): Unit = {
		star2()
	}

	def star1(): Unit = {
		val line = fileLine("Day15.txt")
		val instructions = line.split(',')
		println(instructions.map(hash).sum)
	}

	def star2(): Unit = {
		val line = fileLine("Day15.txt")
		val instructions = line.split(',') map {
			case instructionPattern(label, op, focal) => Instruction(label, hash(label), op(0), focal.toIntOption)
		}
		val boxes = new Array[mutable.LinkedHashMap[String, Int]](256)
		for (i <- boxes.indices) {
			boxes(i) = new mutable.LinkedHashMap[String,Int]
		}
		for (i <- instructions) {
			val boxN = i.box
			if (i.op == '=') {
				boxes(boxN).put(i.label, i.focal.get)
			} else {
				boxes(boxN).remove(i.label)
			}
		}
		var sum = 0
		for (i <- boxes.indices) {
			val box = boxes(i)
			for ((focus, slot) <- box.values.zipWithIndex) {
				sum += (i + 1) * focus * (slot + 1)
			}
		}
		println(sum)
	}

	def hash(in: String): Int = {
		var acc = 0
		for (c <- in) {
			acc += c
			acc *= 17
			acc %= 256
		}
		acc
	}

	val instructionPattern: Regex = """([a-z]+)([=\-])(\d?)""".r
	case class Instruction(label: String, box: Int, op: Char, focal: Option[Int])
}
