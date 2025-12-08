package tyool2023

import grid.{Cell, Direction, DirectionSet, Grid}

object Day16 extends Main {
	def main(args: Array[String]): Unit = {
		star1()
	}

	def star1(): Unit = {
		val lines = fileLines("Day16.txt")
		val grid = Grid(lines)(new Space(_))
		println(grid.render(_.char))
	}

	class Space(val char: Char) {
		var dirs: DirectionSet = DirectionSet.Empty
		def energized: Boolean = !dirs.isEmpty

	}

	/**
	 * A light beam fired from a cell in a direction. This assumes that the origin
	 * cell hasn't been processed by a beam yet.
	 *
	 * @param origin The origin cell
	 * @param direction The direction from the cell
	 */
	case class Beam(origin: Cell[Space], direction: Direction) {
		def fire: Seq[Beam] = {
			origin.value.dirs = direction | origin.value.dirs
			origin.value.char match {
				case '.' => {

				}
				case '/' => {

				}
			}
			???
		}
	}

}
