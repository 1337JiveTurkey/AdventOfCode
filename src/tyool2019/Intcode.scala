package tyool2019

import scala.collection.mutable

class Intcode(init: String) extends Iterator[Option[String]] {
	val mem: mutable.Buffer[Int] = init.split(",").flatMap(_.toIntOption).toBuffer

	// Program counter
	var pc = 0
	var terminated = false
	var error = ""

	override def hasNext: Boolean = !terminated && error.isBlank

	override def next(): Option[String] = {
		Some("Step complete")
	}

	private def decode(position: Int): OpCode = {
		val op = mem(position)
		val instruction = op % 100
		val flags = (op / 100) % 100

		Terminate
	}
}

trait OpCode {
	val code: Int
}

case object Terminate extends OpCode {
	override val code: Int = 99
}