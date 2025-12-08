package tyool2017

import grid.Grid

object Day3 extends Main {
	def main(args: Array[String]): Unit = {
		star1()
	}

	def star1(): Unit = {

		// 312051 is the goal
		val first = new Up(2, 1, 0, 2)
		var next: Arm = first
		for (i <- 1 to 2000) {
			// println(next)
			next = next.next
		}
		println(first.distance(312051))
	}

	/**
	 * An arm of the spiral corecursively generated.
	 */
	trait Arm {
		val first: Int
		val originX: Int
		val originY: Int
		val length: Int

		val next: Arm
		//noinspection ReplaceToWithUntil
		lazy val range: Range = first to first + length - 1

		override def toString: String = {
			s"$originX, $originY $range"
		}

		def distance(n: Int): Int = {
			if (range contains n) {
				val (x, y) = coordinates(n)
				Math.abs(x) + Math.abs(y)
			} else {
				next.distance(n)
			}
		}

		protected def coordinates(n: Int): (Int, Int)
	}

	/*
	First digit increases  >
	Second digit increases ^

	17  16  15  14  13
	18   5   4   3  12
	19   6   1   2  11
	20   7   8   9  10
	21  22  23---> ...
	*/
	class Up(val first: Int, val originX: Int, val originY: Int, val length: Int) extends Arm {
		override lazy val next: Arm = {
			new Left(first + length, originX - 1, originY + length - 1, length)
		}

		override def coordinates(n: Int): (Int, Int) = {
			val offset = n - first
			(originX, originY + offset)
		}
	}

	class Left(val first: Int, val originX: Int, val originY: Int, val length: Int) extends Arm {
		override lazy val next: Arm = {
			new Down(first + length, originX - length + 1, originY - 1, length)
		}

		override def coordinates(n: Int): (Int, Int) = {
			val offset = n - first
			(originX - offset, originY)
		}
	}

	class Down(val first: Int, val originX: Int, val originY: Int, val length: Int) extends Arm {
		override lazy val next: Arm = {
			new Right(first + length, originX + 1, originY - length + 1, length)
		}

		override def coordinates(n: Int): (Int, Int) = {
			val offset = n - first
			(originX, originY - offset)
		}
	}

	class Right(val first: Int, val originX: Int, val originY: Int, val length: Int) extends Arm {
		override lazy val next: Arm = {
			new Up(first + length, originX + length, originY, length + 2)
		}

		override def coordinates(n: Int): (Int, Int) = {
			val offset = n - first
			(originX + offset, originY)
		}
	}

}
