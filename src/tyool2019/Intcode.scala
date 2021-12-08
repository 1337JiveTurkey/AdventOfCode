package tyool2019

import scala.collection.mutable

class Intcode(init: String) extends Iterator[String] {
	val mem: mutable.Buffer[Int] = init.split(",").flatMap(_.toIntOption).toBuffer
	val input: mutable.Queue[Int] = mutable.Queue[Int]()
	val output: mutable.Queue[Int] = mutable.Queue[Int]()

	// We're decoding beforehand so we can play nice with hasNext
	var nextOp: OpCode = decode(0)
	var pc: Int = nextOp.length

	override def hasNext: Boolean = !nextOp.isTerminal

	override def next(): String = {
		val executing = nextOp
		// Keep the same terminal operation
		if (hasNext) {
			nextOp = decode(pc)
			pc += nextOp.length
		}
		executing.op()
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
			case 3 => Input(at, mem(at + 1))
			case 4 => Output(at, mem(at + 1))
			case 99 => Terminate(at)
			case invalid => Invalid(invalid, at)
		}
	}

	// Common parent of opcodes
	abstract class OpCode(val at: Int, val code: Int, val name: String, val length: Int) {
		def header: String = s" $name (@$at): "
		val isTerminal = false;

		def op(): String
	}

	case class Add(override val at: Int, read1: Int, read2: Int, write: Int) extends OpCode(at, 1, "ADD", 4) {
		override def op(): String = {
			val value1 = mem(read1)
			val value2 = mem(read2)
			val result = value1 + value2
			mem(write) = result
			header + s"$result (@$write) = $value1 (@$read1) + $value2 (@$read2)"
		}
	}

	case class Multiply(override val at: Int, read1: Int, read2: Int, write: Int) extends OpCode(at, 2, "MUL", 4) {
		override def op(): String = {
			val value1 = mem(read1)
			val value2 = mem(read2)
			val result = value1 * value2
			mem(write) = result
			header + s"$result (@$write) = $value1 (@$read1) * $value2 (@$read2)"
		}
	}

	case class Input(override val at: Int, write: Int) extends OpCode(at, 3, "INP", 2) {
		override def op(): String = {
			val value = input.dequeue()
			mem(write) = value
			header + s"@$write = $value (Input)"
		}
	}

	case class Output(override val at: Int, read: Int) extends OpCode(at, 4, "OUT", 2) {
		override def op(): String = {
			val value = mem(read)
			output.enqueue(value)
			header + s"(Output) = $value (@$read)"
		}
	}

	case class Terminate(override val at: Int) extends OpCode(at, 99, "TRM", 1) {
		override val isTerminal = true;

		override def op(): String = header + "Terminated"
	}
	case class Invalid(override val at: Int, override val code: Int) extends OpCode(at, code, "XXX", 1) {
		override val isTerminal = true;

		override def op(): String = header + s"Invalid Instruction $code"
	}

	abstract class ParamMode {

	}

	case class PositionMode(at: Int) extends ParamMode {

	}

	case class ImmediateMode(value: Int) extends ParamMode {

	}
}
