package common

import scala.reflect.ClassTag

/**
 * 2D grid for various operations thereon.
 *
 * @param width Width of the grid
 * @param height Height of the grid
 * @tparam T Type of the grid
 */
class Grid[T: ClassTag](val width: Int, val height: Int) extends Iterable[T] {
	val xIndices: Range = 0 until width
	val yIndices: Range = 0 until height

	private val contents = new Array[T](width * height)

	private def onGrid(x: Int, y: Int): Boolean = xIndices.contains(x) && yIndices.contains(y)
	private def address(x: Int, y: Int): Int = {
		if (onGrid(x, y)) {
			x + width * y
		} else {
			throw new IllegalArgumentException(s"$x not in $xIndices or $y not in $yIndices")
		}
	}

	def apply(x: Int, y: Int): T = contents(address(x, y))
	def update(x: Int, y: Int, t: T): Unit = contents(address(x, y)) = t

	override def iterator: Iterator[T] = contents.iterator

	def cell(x: Int, y: Int): Cell[T] = CellImpl(x, y)

	def cells: Iterable[Cell[T]] = {
		for (y <- yIndices; x <- xIndices) yield cell(x, y)
	}

	/**
	 * A container for cell values that includes its coordinates, ability to see
	 * its neighbors and other common extensions.
	 *
	 * @param x The x coordinate of the cell
	 * @param y The y coordinate of the cell
	 */
	private case class CellImpl(x: Int, y: Int) extends Cell[T] {
		def value: T = contents(address(x, y))

		def value_=(t: T): Unit = contents(address(x, y)) = t

		def get(d: Direction): Option[Cell[T]] = {
			if (onGrid(x + d.dx, y + d.dy)) {
				Some(cell(x + d.dx, y + d.dy))
			} else {
				None
			}
		}

		/**
		 *
		 * @return True if this is on any edge of the grid
		 */
		def onEdge: Boolean = {
			x == 0 || y == 0 || x == width - 1 || y == height - 1
		}

		/**
		 *
		 * @return The set of directions that are still on the grid
		 */
		def validDirections: DirectionSet = {
			// Don't bother filtering if there's nothing to filter
			if (!onEdge) {
				DirectionSet.All
			} else {
				DirectionSet.All.filter(d => onGrid(x + d.dx, y + d.dy))
			}
		}
	}
}


object Grid {
	/**
	 * Creates a grid of some object type from a sequence of strings and a
	 * transformation that turns a character at some point in those strings into
	 * an object of some type. This depends on the grid being rectangular but
	 * doesn't enforce it at the moment.
	 *
	 * @param lines The text to turn into a grid
	 * @param transform The transformation from individual characters to objects
	 * @tparam T The type of the grid produced
	 * @return The constructed grid
	 */
	def apply[T: ClassTag](lines: Seq[String])(transform: Char => T): Grid[T] = {
		val height = lines.length
		val width = lines.head.length
		val retVal = new Grid[T](width, height)
		for ((line, y) <- lines.zipWithIndex) {
			for((char, x) <- line.zipWithIndex) {
				retVal(x, y) = transform(char)
			}
		}
		retVal
	}
}

/**
 * An individual cell on a grid with various operations to access its neighbors.
 *
 * @tparam T The type of data stored within the cell.
 */
trait Cell[T] extends Point {
	def value: T

	def value_=(t: T): Unit

	/**
	 * Gets the cell in the given direction
	 *
	 * @param d The direction to get the cell from.
	 * @return Some(cell) if there is a cell in that direction or else None
	 */
	def get(d: Direction): Option[Cell[T]]

	/**
	 *
	 * @return The set of directions that are still on the grid
	 */
	def validDirections: DirectionSet

	/**
	 * Gets all the cells in a given direction to the edge of the grid
	 *
	 * @param d The direction to look for cells
	 * @return All cells in the direction given, from nearest to furthest
	 */
	def ray(d: Direction): List[Cell[T]] = {
		val cell = get(d)
		if (cell.isDefined) {
			val tail = cell.get.ray(d)
			cell.get :: tail
		} else {
			List.empty
		}
	}

	/**
	 * Gets all the neighbors in the given directions.
	 *
	 * @param dirs The DirectionSet to get the cells in the direction of
	 * @return The cells adjacent to the current cell.
	 */
	def neighbors(dirs: DirectionSet): List[Cell[T]] = {
		var cells: List[Cell[T]] = List.empty
		(validDirections & dirs).foreach(dir => {
			cells = get(dir).get :: cells
		})
		cells
	}

	def neighbors: List[Cell[T]] = neighbors(DirectionSet.All)
}