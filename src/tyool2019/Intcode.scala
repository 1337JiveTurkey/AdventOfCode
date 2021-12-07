package tyool2019

import scala.collection.mutable

class Intcode(init: String) extends Iterator[String] {
	val mem: mutable.Buffer[Int] = init.split(",").flatMap(_.toIntOption).toBuffer

	// Program counter
	var pc = 0
	var nextOp: OpCode = null;
	{
		val (newOp, pcIncrement) = decode(pc)
		pc += pcIncrement
		nextOp = newOp
	}

	override def hasNext: Boolean = !nextOp.isTerminal

	override def next(): String = {
		val executing = nextOp
		val (newOp, pcIncrement) = decode(pc)
		pc += pcIncrement
		nextOp = newOp
		executing.op(mem)
	}

	private def decode(at: Int): (OpCode, Int) = {
		val op = mem(at)
		val instruction = op % 100
		val flags = (op / 100) % 100

		instruction match {
			case 1 => (Add(at, mem(at + 1), mem(at + 2), mem(at + 3)), 4)
			case 2 => (Mul(at, mem(at + 1), mem(at + 2), mem(at + 3)), 4)
			case 99 => (Terminate(at), 1)
			case invalid => (Invalid(invalid, at), 1)
		}
	}

	// Common parent of opcodes
	trait OpCode {
		val at: Int
		val code: Int
		val name: String
		def header: String = s" $name (@$at): "
		val isTerminal = false;

		def op(mem: mutable.Buffer[Int]): String
	}

	case class Add(at: Int, read1: Int, read2: Int, write: Int) extends OpCode {
		val code = 1
		val name = "ADD"

		override def op(mem: mutable.Buffer[Int]): String = {
			val value1 = mem(read1)
			val value2 = mem(read2)
			mem(write) = value1 + value2
			header + s"@$write = $value1 (@$read1) + $value2 (@$read2)"
		}
	}

	case class Mul(at: Int, read1: Int, read2: Int, write: Int) extends OpCode {
		val code = 2
		val name = "MUL"

		override def op(mem: mutable.Buffer[Int]): String = {
			val value1 = mem(read1)
			val value2 = mem(read2)
			mem(write) = value1 * value2
			header + s"@$write = $value1 (@$read1) * $value2 (@$read2)"
		}
	}

	case class Terminate(at: Int) extends OpCode {
		val code = 99
		val name = "TRM"
		override val isTerminal = true;

		override def op(mem: mutable.Buffer[Int]): String = header + "Terminated"
	}
	case class Invalid(code: Int, at: Int) extends OpCode {
		val name = "XXX"
		override val isTerminal = true;

		override def op(mem: mutable.Buffer[Int]): String = header + s"Invalid Instruction $code"
	}

}
