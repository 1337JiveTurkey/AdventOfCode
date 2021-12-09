package tyool2021

object Day8 extends Main {
	def main(args: Array[String]): Unit = {
		star15()
	}

	def star15(): Unit = {
		val lines = fileLines("Star15.txt")
		var total = 0
		for (line <- lines) {
			val parts = line.split("""\|""")
			val output = parts(1)
			total += output.
				split("\\s").
				map(_.length).
				count(x => x == 2 || x == 3 || x == 4 || x == 7)
		}
		println(total)
	}

	def star16(): Unit = {

	}

	/*
	 * Unscrambling algorithm notes.
	 * 1: 2 segments
	 * 7: 3 segments
	 * 4: 4 segments
	 * 2, 3, 5: 5 segments
	 * 0, 6, 9: 6 segments
	 * 8: 7 Segments
	 *
	 * First get the four unique ones.
	 * 9 is only 6 digit superset of 4
	 * 0 is superset of 4 but not of 7
	 * 6 is not a superset of either
	 *
	 * 3 is only 5 digit superset of 7
	 * 5 is only 5 digit subset of 6
	 * 2 is all that's left
	 *
	 */

	case class Digit(digit: Int, chars: String) {
		val segments: Set[Char] = Set.from(chars)

	}
	case object Zero  extends Digit(0,"abcefg")
	case object One   extends Digit(1,"cf")
	case object Two   extends Digit(2,"acdeg")
	case object Three extends Digit(3,"acdfg")
	case object Four  extends Digit(4,"bcdf")
	case object Five  extends Digit(5,"abdfg")
	case object Six   extends Digit(6,"abdefg")
	case object Seven extends Digit(7,"acf")
	case object Eight extends Digit(8,"abcdefg")
	case object Nine  extends Digit(9,"abcdfg")

	case class Segment(char: Char) {

	}
	case object A extends Segment('a')
	case object B extends Segment('b')
	case object C extends Segment('c')
	case object D extends Segment('d')
	case object E extends Segment('e')
	case object F extends Segment('f')
	case object G extends Segment('g')

	def letterToSegment(char: Char): Segment = {
		char match {
			case 'a' => A
			case 'b' => B
			case 'c' => C
			case 'd' => D
			case 'e' => E
			case 'f' => F
			case 'g' => G
		}
	}
}
