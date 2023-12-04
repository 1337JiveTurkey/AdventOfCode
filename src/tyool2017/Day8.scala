package tyool2017

import scala.collection.mutable
import scala.util.matching.Regex;

object Day8 extends Main {
	val instructionPattern: Regex = """([a-z]+) (inc|dec) (-?\d+) if ([a-z]+) (>=|>|<|<=|==|!=) (-?\d+)""".r

	def main(args: Array[String]): Unit = {
		star2()
	}

	def star1(): Unit = {
		val lines = fileLines("Day8.txt")
		val instructions = lines map {
			case instructionPattern(r1, op1, v1, r2, op2, v2) =>
				Instruction(r1, if (op1 == "inc") v1.toInt else -v1.toInt, Condition(r2, op2, v2.toInt))
		}
		val registers = new mutable.HashMap[String, Int]()
		for (instruction <- instructions) {
			instruction.operate(registers)
		}
		println(registers)
	}

	def star2(): Unit = {
		val lines = fileLines("Day8.txt")
		val instructions = lines map {
			case instructionPattern(r1, op1, v1, r2, op2, v2) =>
				Instruction(r1, if (op1 == "inc") v1.toInt else -v1.toInt, Condition(r2, op2, v2.toInt))
		}
		val registers = new mutable.HashMap[String, Int]()
		var maxResult = 0
		for (instruction <- instructions) {
			val result = instruction.operate(registers)
			if (maxResult < result) {
				maxResult = result
			}
		}
		println(maxResult)
	}

	case class Instruction(reg: String, value: Int, cond: Condition) {
		def operate(registers: mutable.Map[String, Int]): Int = {
			if (cond.test(registers)) {
				val oldValue = registers.getOrElse(reg, 0)
				registers.put(reg, oldValue + value)
				oldValue + value
			} else {
				0
			}
		}
	}

	case class Condition(reg: String, op: String, value: Int) {
		def test(registers: mutable.Map[String, Int]): Boolean = {
			val testValue = registers.getOrElse(reg, 0)
			op match {
				case "<" => testValue < value
				case "<=" => testValue <= value
				case ">" => testValue > value
				case ">=" => testValue >= value
				case "==" => testValue == value
				case "!=" => testValue != value
			}
		}
	}
}
