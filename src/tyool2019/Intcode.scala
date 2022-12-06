package tyool2019

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Intcode(init: String) extends Iterator[String] {
	val mem: mutable.Buffer[Int] = {
		val source = init.split(",").flatMap(_.toIntOption).toBuffer
		// Include an extra 1000 ints of space
		source.appendAll(new ArrayBuffer[Int](1000))
	}
	val input: mutable.Queue[Int] = mutable.Queue[Int]()
	val output: mutable.Queue[Int] = mutable.Queue[Int]()

	var pc: Int = 0

	var terminated = false

	override def hasNext: Boolean = !terminated

	override def next(): String = {
		if (terminated) {
			throw new IllegalStateException("Intcode instance is terminated")
		}
		var nextOp = decode(pc)
		pc += nextOp.length
		if (nextOp.isTerminal) {
			terminated = true
		}
		nextOp.op()
	}

	private def decode(at: Int): OpCode = {
		val op = mem(at)
		val instruction = op % 100
		val flag1 = (op / 100) % 10
		val flag2 = (op / 1000) % 10
		val flag3 = (op / 10000) % 10

		val param1 = decodeParameter(flag1, at + 1)
		val param2 = decodeParameter(flag2, at + 2)
		val param3 = decodeParameter(flag3, at + 3)

		instruction match {
			case 1 => Add(at, param1, param2, param3)
			case 2 => Multiply(at, param1, param2, param3)
			case 3 => Input(at, param1)
			case 4 => Output(at, param1)
			case 99 => Terminate(at)
			case invalid => Invalid(at, invalid)
		}
	}

	private def decodeParameter(flag: Int, at: Int): ParamMode = {
		flag match {
			case 0 => PositionMode(mem(at))
			case 1 => ImmediateMode(mem(at))
			case invalid => InvalidMode(invalid)
		}
	}

	// Common parent of opcodes
	abstract class OpCode(val at: Int, val code: Int, val name: String, val length: Int) {
		def header: String = s" $name (@$at): "
		val isTerminal = false;

		/**
		 * Performs the operation of the OpCode and returns a string representation.
		 *
		 * @return String representation of what the operation is doing.
		 */
		def op(): String
	}

	case class Add(override val at: Int, read1: ParamMode, read2: ParamMode, write: ParamMode) extends OpCode(at, 1, "ADD", 4) {
		override def op(): String = {
			val value1 = read1.read()
			val value2 = read2.read()
			val result = value1 + value2
			write.write(result)
			header + s"$result (@$write) = $value1 (@$read1) + $value2 (@$read2)"
		}
	}

	case class Multiply(override val at: Int, read1: ParamMode, read2: ParamMode, write: ParamMode) extends OpCode(at, 2, "MUL", 4) {
		override def op(): String = {
			val value1 = read1.read()
			val value2 = read2.read()
			val result = value1 * value2
			write.write(result)
			header + s"$result (@$write) = $value1 (@$read1) * $value2 (@$read2)"
		}
	}

	case class Input(override val at: Int, write: ParamMode) extends OpCode(at, 3, "INP", 2) {
		override def op(): String = {
			val value = input.dequeue()
			write.write(value)
			header + s"@$write = $value (Input)"
		}
	}

	case class Output(override val at: Int, read: ParamMode) extends OpCode(at, 4, "OUT", 2) {
		override def op(): String = {
			val value = read.read()
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
		def read(): Int
		def write(value: Int): Unit
	}

	case class PositionMode(at: Int) extends ParamMode {
		override def read(): Int = mem(at)

		override def write(value: Int): Unit = mem(at) = value
	}

	case class ImmediateMode(value: Int) extends ParamMode {
		override def read(): Int = value

		override def write(value: Int): Unit = throw new NoSuchMethodException("Can't write an immediate parameter")
	}

	case class InvalidMode(flag: Int) extends ParamMode {
		override def read(): Int = throw new NoSuchMethodException("Can't read an invalid parameter")

		override def write(value: Int): Unit = throw new NoSuchMethodException("Can't write an invalid parameter")
	}
}
