package tyool2024

import common.{Direction, DirectionSet, Grid, North}

object Day6 extends Main {
	def main(args: Array[String]): Unit = {
		star1()
	}

	def star1(): Unit = {
		val lines = fileLines("Day6.txt")
		val grid = Grid[Square](lines) {
			case '.' => Space
			case '#' => Wall
			case '^' => Visited(DirectionSet(North))
		}
		val start = grid.cells.find(_.value.isInstanceOf[Visited]).get
		var currentCell = start
		var currentDirection: Direction = North
		while (currentCell.validDirections.contains(currentDirection)) {
			val next = currentCell.get(currentDirection).get
			next.value match {
				case Wall =>
					currentDirection = currentDirection.relative(common.Right)
					currentCell.value = Visited(DirectionSet(currentDirection))
				case Space =>
					currentCell = next
					currentCell.value = Visited(DirectionSet(currentDirection))
				case Visited(ds) =>
					currentCell = next
					currentCell.value = Visited(DirectionSet(currentDirection) | ds)
			}
		}
		println(grid.render)

		println(grid.cells.count(_.value.isInstanceOf[Visited]))
	}

	def star2(): Unit = {
		val lines = fileLines("Day6.txt")
		val grid = Grid[Square](lines) {
			case '.' => Space
			case '#' => Wall
			case '^' => Visited(DirectionSet(North))
		}
		val start = grid.cells.find(_.value.isInstanceOf[Visited]).get
		var currentCell = start
		var currentDirection: Direction = North
		while (currentCell.validDirections.contains(currentDirection)) {
			val next = currentCell.get(currentDirection).get
			next.value match {
				case Wall =>
					currentDirection = currentDirection.relative(common.Right)
					currentCell.value = Visited(DirectionSet(currentDirection))
				case Space =>
					currentCell = next
					currentCell.value = Visited(DirectionSet(currentDirection))
				case Visited(ds) =>
					if (ds.contains(currentDirection)) {
						// Found a loop
					}
					currentCell = next
					currentCell.value = Visited(DirectionSet(currentDirection) | ds)
			}
		}
		println(grid.render)
		println(grid.cells.count(_.value.isInstanceOf[Visited]))
	}

	def getTouchedSquares(grid: Grid[Square]): Grid[Boolean] = {
		???
	}

	sealed trait Square
	case object Wall extends Square
	case object Space extends Square
	case class Visited(ds: DirectionSet) extends Square

	/**
	 * Implicit converter for the Grid.render function.
	 *
	 * @param s The square to render.
	 * @return The character the square will be rendered with.
	 */
	implicit def renderSquare(s: Square): Char = {
		s match {
			case Wall => '#'
			case Space => '.'
			case Visited(_) => 'X'
		}
	}
}
