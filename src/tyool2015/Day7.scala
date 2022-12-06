package tyool2015

import scala.collection.mutable
import scala.util.matching.Regex

object Day7 extends Main {
	def main(args: Array[String]): Unit = {
		star2()
	}

	val AndPattern: Regex =       """(\w+) AND (\w+) -> (\w+)""".r
	val OrPattern: Regex =        """(\w+) OR (\w+) -> (\w+)""".r
	val NotPattern: Regex =       """NOT (\w+) -> (\w+)""".r
	val ImmediatePattern: Regex = """(\w+) -> (\w+)""".r
	val LShiftPattern: Regex =    """(\w+) LSHIFT (\w+) -> (\w+)""".r
	val RShiftPattern: Regex =    """(\w+) RSHIFT (\w+) -> (\w+)""".r

	val map: mutable.Map[String, Circuit] = mutable.HashMap.empty[String, Circuit]

	def star1(): Unit = {
		val lines = fileLines("Day7.txt")

		for (line <- lines) {
			val circuit = matchLine(line)
			map += circuit.name -> circuit
		}
		println(map("a").value)
	}

	def star2(): Unit = {
		val lines = fileLines("Day7.txt")

		for (line <- lines) {
			val circuit = matchLine(line)
			map += circuit.name -> circuit
		}
		map += "b" -> ImmediateCircuit("b", ImmediateSource(46065))
		println(map("a").value)
	}

	def matchLine(line: String): Circuit = line match {
		case AndPattern(src1, src2, dst) =>
			AndCircuit(dst, source(src1), source(src2))
		case OrPattern(src1, src2, dst) =>
			OrCircuit(dst, source(src1), source(src2))
		case NotPattern(src, dst) =>
			NotCircuit(dst, source(src))
		case ImmediatePattern(src, dst) =>
			ImmediateCircuit(dst, source(src))
		case LShiftPattern(src1, src2, dst) =>
			LeftShiftCircuit(dst, source(src1), source(src2))
		case RShiftPattern(src1, src2, dst) =>
			RightShiftCircuit(dst, source(src1), source(src2))
	}

	trait Circuit {
		val name: String
		val value: Int
	}

	case class AndCircuit(name: String, src1: Source, src2: Source) extends Circuit {
		override lazy val value: Int = (src1() & src2()) & 0xFFFF
	}
	case class OrCircuit(name: String, src1: Source, src2: Source) extends Circuit {
		override lazy val value: Int = (src1() | src2()) & 0xFFFF
	}
	case class NotCircuit(name: String, src1: Source) extends Circuit {
		override lazy val value: Int = (~src1()) & 0xFFFF
	}
	case class ImmediateCircuit(name: String, src1: Source) extends Circuit {
		override lazy val value: Int = src1() & 0xFFFF
	}
	case class LeftShiftCircuit(name: String, src1: Source, src2: Source) extends Circuit {
		override lazy val value: Int = (src1() << src2()) & 0xFFFF
	}
	case class RightShiftCircuit(name: String, src1: Source, src2: Source) extends Circuit {
		override lazy val value: Int = (src1() >> src2()) & 0xFFFF
	}

	trait Source {
		def apply(): Int
	}

	def source(string: String): Source = {
		string.toIntOption.map(ImmediateSource).getOrElse(ReferenceSource(string))
	}

	case class ImmediateSource(apply: Int) extends Source
	case class ReferenceSource(name: String) extends Source {
		def apply(): Int = {
			map(name).value
		}
	}
}
