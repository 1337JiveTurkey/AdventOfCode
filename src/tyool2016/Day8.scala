package tyool2016

import grid.Grid

import scala.util.matching.Regex

object Day8 extends Main {
	def main(args: Array[String]): Unit = {
		star2()
	}

	val rectLine: Regex = """rect (\d+)x(\d+)""".r
	val rotateRowLine: Regex = """rotate row y=(\d+) by (\d+)""".r
	val rotateColumnLine: Regex = """rotate column x=(\d+) by (\d+)""".r

	def star1(): Unit = {
		def lines = fileLines("Day8.txt")
		def commands: IndexedSeq[Command] = lines map {
				case rectLine(x, y) => Rect(x.toInt, y.toInt)
				case rotateRowLine(y, by) => RotRow(y.toInt, by.toInt)
				case rotateColumnLine(x, by) => RotCol(x.toInt, by.toInt)
		}
		var screen = new Grid[Boolean](50, 6)
		for (command <- commands) {
			screen = command(screen)
		}
		println(screen.count(identity))
	}

	def star2(): Unit = {
		def lines = fileLines("Day8.txt")

		def commands: IndexedSeq[Command] = lines map {
			case rectLine(x, y) => Rect(x.toInt, y.toInt)
			case rotateRowLine(y, by) => RotRow(y.toInt, by.toInt)
			case rotateColumnLine(x, by) => RotCol(x.toInt, by.toInt)
		}

		var screen = new Grid[Boolean](50, 6)
		for (command <- commands) {
			screen = command(screen)
		}
		println(screen.render(Grid.renderBoolean))
	}

	trait Command {
		def apply(screen: Grid[Boolean]): Grid[Boolean]
	}

	case class Rect(width: Int, height: Int) extends Command {
		def apply(screen: Grid[Boolean]): Grid[Boolean] = {
			for (y <- 0 until height) {
				for (x <- 0 until width) {
					screen(x, y) = true
				}
			}
			screen
		}
	}

	case class RotRow(y: Int, by: Int) extends Command {
		def apply(screen: Grid[Boolean]): Grid[Boolean] = {
			val newScreen = new Grid[Boolean](50, 6)
			for (cell <- screen.cells) {
				if (cell.y == y) {
					newScreen((cell.x + by) % 50, cell.y) = cell.value
				} else {
					newScreen(cell.x, cell.y) = cell.value
				}
			}
			newScreen
		}
	}

	case class RotCol(x: Int, by: Int) extends Command {
		def apply(screen: Grid[Boolean]): Grid[Boolean] = {
			val newScreen = new Grid[Boolean](50, 6)
			for (cell <- screen.cells) {
				if (cell.x == x) {
					newScreen(cell.x, (cell.y + by) % 6) = cell.value
				} else {
					newScreen(cell.x, cell.y) = cell.value
				}
			}
			newScreen
		}
	}

}
