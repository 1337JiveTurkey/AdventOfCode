package tyool2022

import scala.collection.immutable.TreeMap
import scala.collection.mutable
import scala.util.matching.Regex

object Day5 extends Main {
	def main(args: Array[String]): Unit = {
		star2()
	}

	def star1(): Unit = {
		val lines = fileLines("Day5.txt")
		val halves = splitOnBlanks(lines)
		// Flip the configuration section upside down for the next part
		val (configuration, instructions) = (halves.head.reverse, halves.tail.head)
		val boxes = stackBoxes(configuration)
		val instrs = readInstructions(instructions)
		for (i <- instrs) {
			i.move9000(boxes)
		}
		println(boxes)
	}

	def star2(): Unit = {
		val lines = fileLines("Day5.txt")
		val halves = splitOnBlanks(lines)
		// Flip the configuration section upside down for the next part
		val (configuration, instructions) = (halves.head.reverse, halves.tail.head)
		val boxes = stackBoxes(configuration)
		val instrs = readInstructions(instructions)
		for (i <- instrs) {
			i.move9001(boxes)
		}
		println(boxes)
	}

	def stackBoxes(configuration: IndexedSeq[String]): Map[String, BoxStack] = {
		val numbers = configuration.head
		val boxes = configuration.tail

		val mb = TreeMap.newBuilder[String, BoxStack]
		for (i <- numbers.indices) {
			if (numbers(i).isLetterOrDigit) {
				val index = numbers(i).toString
				val position = i
				val boxStack = new BoxStack(index, boxes, position)
				mb.addOne(index, boxStack)
			}
		}
		mb.result()
	}

	class BoxStack(val number: String, rows: IndexedSeq[String], position: Int) {
		val stack: mutable.Stack[Char] = mutable.Stack.empty[Char]
		for (row <- rows) {
			if (row.indices.contains(position) && row.charAt(position).isLetterOrDigit) {
				push(row.charAt(position))
			}
		}

		def push(char: Char): Unit = stack.push(char)
		def pop(): Char = stack.pop()

		override def toString: String = {
			if (stack.isEmpty) {
				" "
			} else {
				stack.top.toString
			}
		}
	}

	val LinePattern: Regex = """move (\d+) from (\d) to (\d)""".r

	def readInstructions(instructions: IndexedSeq[String]): IndexedSeq[Instruction] = {
		val isb = IndexedSeq.newBuilder[Instruction]
		for (LinePattern(count, from, to) <- instructions) {
			isb.addOne(Instruction(count.toInt, from, to))
		}
		isb.result()
	}

	case class Instruction(count: Int, from: String, to: String) {
		def move9000(boxes: Map[String, BoxStack]): Unit = {
			for (c <- 1 to count) {
				boxes(to).push(boxes(from).pop())
			}
		}

		def move9001(boxes: Map[String, BoxStack]): Unit = {
			// Efficiency!
			val stack: mutable.Stack[Char] = mutable.Stack.empty[Char]
			for (c <- 1 to count) {
				stack.push(boxes(from).pop())
			}
			for (c <- 1 to count) {
				boxes(to).push(stack.pop())
			}
		}
	}
}
