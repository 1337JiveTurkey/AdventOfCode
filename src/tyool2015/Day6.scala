package tyool2015

import common.Grid

import scala.util.matching.Regex

object Day6 extends Main {
	def main(args: Array[String]): Unit = {
		star1()
	}
	val turnOn: Regex = """turn on (\d+),(\d+) through (\d+),(\d+)""".r
	val turnOff: Regex = """turn off (\d+),(\d+) through (\d+),(\d+)""".r
	val toggle: Regex = """toggle (\d+),(\d+) through (\d+),(\d+)""".r

	trait Instruction {
		val x1: Int
		val y1: Int
		val x2: Int
		val y2: Int

		def operate(lights: Grid[Boolean]): Unit = {
			for (x <- x1 to x2) {
				for (y <- y1 to y2) {
					val current = lights(x, y)
					lights(x, y) = operateCell(current)
				}
			}
		}

		def operateCell(value: Boolean): Boolean
	}

	case class TurnOn(x1: Int, y1: Int, x2: Int, y2: Int) extends Instruction {
		override def operateCell(value: Boolean): Boolean = true
	}
	case class TurnOff(x1: Int, y1: Int, x2: Int, y2: Int) extends Instruction {
		override def operateCell(value: Boolean): Boolean = false
	}
	case class Toggle(x1: Int, y1: Int, x2: Int, y2: Int) extends Instruction {
		override def operateCell(value: Boolean): Boolean = !value
	}

	def parseLine(line: String): Instruction = line match {
		case turnOn(x1, y1, x2, y2) => TurnOn(x1.toInt, y1.toInt, x2.toInt, y2.toInt)
		case turnOff(x1, y1, x2, y2) => TurnOff(x1.toInt, y1.toInt, x2.toInt, y2.toInt)
		case toggle(x1, y1, x2, y2) => Toggle(x1.toInt, y1.toInt, x2.toInt, y2.toInt)
	}

	def star1(): Unit = {
		val lines = fileLines("Day6.txt")
		val instructions = lines map parseLine
		val lights = new Grid[Boolean](1000, 1000)
		for (instruction <- instructions) {
			instruction.operate(lights)
		}
		val total: Int = lights.contents.count(_ == true)
		println(total)
	}
}
