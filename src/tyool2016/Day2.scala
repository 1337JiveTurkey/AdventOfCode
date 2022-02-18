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
	case object One extends Button {
		val Up: Button = One
		val Down: Button = Four
		val Left: Button = One
		val Right: Button = Two
	}
	case object Two extends Button {
		val Up: Button = Two
		val Down: Button = Five
		val Left: Button = One
		val Right: Button = Three
	}
	case object Three extends Button {
		val Up: Button = Three
		val Down: Button = Six
		val Left: Button = Two
		val Right: Button = Three
	}
	case object Four extends Button {
		val Up: Button = One
		val Down: Button = Seven
		val Left: Button = Four
		val Right: Button = Five
	}
	case object Five extends Button {
		val Up: Button = Two
		val Down: Button = Eight
		val Left: Button = Four
		val Right: Button = Six
	}
	case object Six extends Button {
		val Up: Button = Three
		val Down: Button = Nine
		val Left: Button = Five
		val Right: Button = Six
	}
	case object Seven extends Button {
		val Up: Button = Four
		val Down: Button = Seven
		val Left: Button = Seven
		val Right: Button = Eight
	}
	case object Eight extends Button {
		val Up: Button = Five
		val Down: Button = Eight
		val Left: Button = Seven
		val Right: Button = Nine
	}
	case object Nine extends Button {
		val Up: Button = Six
		val Down: Button = Nine
		val Left: Button = Eight
		val Right: Button = Nine
	}
}
