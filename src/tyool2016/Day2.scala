package tyool2016

object Day2 extends Main {
	def main(args: Array[String]): Unit = {
		star1()
	}

	def star1(): Unit = {
		val lines = fileLines("Day2.txt")
		for (line <- lines) {
			println(line.foldLeft(Five: Button)((button, char) => button.successor(char)))
		}
	}

	trait Button {
		def successor(c: Char): Button = {
			if (c == 'U') {
				Up
			} else if (c == 'D') {
				Down
			} else if (c == 'L') {
				Left
			} else {
				Right
			}
		}
		def Up: Button
		def Down: Button
		def Left: Button
		def Right: Button
	}
	/*
	    1
    2 3 4
  5 6 7 8 9
    A B C
      D
	 */

	case object One extends Button {
		val Up: Button = One
		val Down: Button = Three
		val Left: Button = One
		val Right: Button = One
	}
	case object Two extends Button {
		val Up: Button = Two
		val Down: Button = Six
		val Left: Button = Two
		val Right: Button = Three
	}
	case object Three extends Button {
		val Up: Button = One
		val Down: Button = Seven
		val Left: Button = Two
		val Right: Button = Four
	}
	case object Four extends Button {
		val Up: Button = Four
		val Down: Button = Eight
		val Left: Button = Three
		val Right: Button = Four
	}
	case object Five extends Button {
		val Up: Button = Five
		val Down: Button = Five
		val Left: Button = Five
		val Right: Button = Six
	}
	case object Six extends Button {
		val Up: Button = Two
		val Down: Button = A
		val Left: Button = Five
		val Right: Button = Seven
	}
	case object Seven extends Button {
		val Up: Button = Three
		val Down: Button = B
		val Left: Button = Six
		val Right: Button = Eight
	}
	case object Eight extends Button {
		val Up: Button = Four
		val Down: Button = C
		val Left: Button = Seven
		val Right: Button = Nine
	}
	case object Nine extends Button {
		val Up: Button = Nine
		val Down: Button = Nine
		val Left: Button = Eight
		val Right: Button = Nine
	}
	case object A extends Button {
		val Up: Button = Six
		val Down: Button = A
		val Left: Button = A
		val Right: Button = A
	}
	case object B extends Button {
		val Up: Button = Seven
		val Down: Button = D
		val Left: Button = A
		val Right: Button = C
	}
	case object C extends Button {
		val Up: Button = Eight
		val Down: Button = C
		val Left: Button = B
		val Right: Button = C
	}
	case object D extends Button {
		val Up: Button = B
		val Down: Button = D
		val Left: Button = D
		val Right: Button = D
	}

}
