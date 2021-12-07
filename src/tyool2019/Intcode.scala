package tyool2019

import scala.collection.mutable

class Intcode(init: String) extends Iterator[String] {
	val mem: mutable.Buffer[Int] = init.split(",").flatMap(_.toIntOption).toBuffer

	var nextOp: OpCode = decode(0)
	var pc: Int = nextOp.length

	override def hasNext: Boolean = !nextOp.isTerminal

	override def next(): String = {
		val executing = nextOp
		nextOp = decode(pc)
		pc += nextOp.length
		executing.op(mem)
	}

	private def decode(at: Int): OpCode = {
		val op = mem(at)
		val instruction = op % 100
		val flag1 = (op / 100) % 10
		val flag2 = (op / 1000) % 10
		val flag3 = (op / 10000) % 10

		instruction match {
			case 1 => Add(at, mem(at + 1), mem(at + 2), mem(at + 3))
			case 2 => Multiply(at, mem(at + 1), mem(at + 2), mem(at + 3))
			case 99 => Terminate(at)
			case invalid => Invalid(invalid, at)
		}
	}

	// Common parent of opcodes
	trait OpCode {
		val at: Int
		val code: Int
		val name: String
		def header: String = s" $name (@$at): "
		val isTerminal = false;
		val length: Int

		def op(mem: mutable.Buffer[Int]): String
	}

	case class Add(at: Int, read1: Int, read2: Int, write: Int) extends OpCode {
		val code = 1
		val name = "ADD"
		val length = 4

		override def op(mem: mutable.Buffer[Int]): String = {
			val value1 = mem(read1)
			val value2 = mem(read2)
			mem(write) = value1 + value2
			header + s"@$write = $value1 (@$read1) + $value2 (@$read2)"
		}
	}

	case class Multiply(at: Int, read1: Int, read2: Int, write: Int) extends OpCode {
		val code = 2
		val name = "MUL"
		val length = 4

		override def op(mem: mutable.Buffer[Int]): String = {
			val value1 = mem(read1)
			val value2 = mem(read2)
			mem(write) = value1 * value2
			header + s"@$write = $value1 (@$read1) * $value2 (@$read2)"
		}
	}

	case class Input(at: Int, write: Int) extends OpCode {
		override val code: Int = 3
		override val name: String = "INP"
		override val length: Int = 2

		override def op(mem: mutable.Buffer[Int]): String = {
			???
		}
	}

	case class Output(at: Int, read: Int) extends OpCode {
		override val code: Int = 4
		override val name: String = "OUT"
		override val length: Int = 2

		override def op(mem: mutable.Buffer[Int]): String = {
			???
		}
	}

	case class Terminate(at: Int) extends OpCode {
		val code = 99
		val name = "TRM"
		override val isTerminal = true;
		val length = 1

		override def op(mem: mutable.Buffer[Int]): String = header + "Terminated"
	}
	case class Invalid(code: Int, at: Int) extends OpCode {
		val name = "XXX"
		override val isTerminal = true;
		val length = 1

		override def op(mem: mutable.Buffer[Int]): String = header + s"Invalid Instruction $code"
	}

}
