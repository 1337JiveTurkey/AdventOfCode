package tyool2023

import common.Grid

object Day23 extends Main {
	def main(args: Array[String]): Unit = {
		star1()
	}

	def star1(): Unit = {
		val lines = fileLines("Day23.txt")
		val grid = Grid(lines) {
			case '#' => Tree
			case '.' => Path
			case '>' => SlopeRight
			case 'v' => SlopeDown
		}

	}

	sealed trait Terrain
	case object Tree extends Terrain
	case object Path extends Terrain
	case object SlopeRight extends Terrain
	case object SlopeDown extends Terrain


}
