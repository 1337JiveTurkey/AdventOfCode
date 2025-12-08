package grid

import scala.language.implicitConversions
import scala.reflect.ClassTag

/**
 * 2D grid for various operations thereon.
 *
 * @param width Width of the grid
 * @param height Height of the grid
 * @tparam T Type of the grid
 */
class Grid[T: ClassTag](val width: Int, val height: Int, val edges: EdgeBehavior = WrapNeither) {
	val xIndices: Range = 0 until width
	val yIndices: Range = 0 until height

	private val ts = new Array[T](width * height)

	private def wrapX(x: Int): Int = {
		if (edges.wrapX) {
			(x + width) % width
		} else if (xIndices.contains(x)) {
			x
		} else {
			throw new IllegalArgumentException(s"x $x not in $xIndices")
		}
	}

	private def wrapY(y: Int): Int = {
		if (edges.wrapY) {
			(y + height) % height
		} else if (yIndices.contains(y)) {
			y
		} else {
			throw new IllegalArgumentException(s"y $y not in $yIndices")
		}
	}

	private def cellOnEdge(x: Int, y: Int): Boolean = {
		!edges.wrapX && (x == 0 || x == width - 1) || !edges.wrapY && (y == 0 || y == height - 1)
	}

	private def cellOnGrid(x: Int, y: Int): Boolean = {
		(edges.wrapX || xIndices.contains(x)) && (edges.wrapY || yIndices.contains(y))
	}
	private def address(x: Int, y: Int): Int = wrapX(x) + width * wrapY(y)

	def apply(x: Int, y: Int): T = ts(address(x, y))
	def update(x: Int, y: Int, t: T): Unit = ts(address(x, y)) = t

	def contents: Iterable[T] = ts

	def mapCells[R: ClassTag](op: Cell[T] => R): Grid[R] = {
		val retVal = new Grid[R](width, height, edges)
		for ((cell, i) <- cells.zipWithIndex) {
			retVal.ts(i) = op(cell)
		}
		retVal
	}

	def cell(x: Int, y: Int): Cell[T] = CellImpl(wrapX(x), wrapY(y))

	def cells: Iterable[Cell[T]] = {
		for (y <- yIndices; x <- xIndices) yield cell(x, y)
	}

	def withEdges(edgeBehavior: EdgeBehavior): Grid[T] = {
		val retVal = new Grid[T](width, height, edgeBehavior)
		System.arraycopy(ts, 0, retVal.ts, 0, ts.length)
		retVal
	}

	def render(implicit converter: T => Char): String = {
		val sb = new StringBuilder

		for (y <- yIndices) {
			for (x <- xIndices) {
				sb.append(converter(apply(x, y)))
			}
			sb.append('\n')
		}
		sb.result()
	}

	/**
	 * Generates a subgrid from arbitrary set of x and y indices.
	 * @param fromXIndices The x indices to turn into the subgrid.
	 * @param fromYIndices The y indices to turn into the subgrid.
	 * @return The subgrid containing the points from the indices.
	 */
	def subGrid(fromXIndices: Iterable[Int], fromYIndices: Iterable[Int]): Grid[T] = {
		val retVal = new Grid[T](fromXIndices.size, fromYIndices.size)
		for ((fromX, toX) <- fromXIndices.zipWithIndex; (fromY, toY) <- fromYIndices.zipWithIndex) {
			retVal(toX, toY) = apply(fromX, fromY)
		}
		retVal
	}

	/**
	 * A container for cell values that includes its coordinates, ability to see
	 * its neighbors and other common extensions.
	 *
	 * @param x The x coordinate of the cell
	 * @param y The y coordinate of the cell
	 */
	private case class CellImpl(x: Int, y: Int) extends Cell[T] {
		def value: T = apply(x, y)

		def value_=(t: T): Unit = update(x, y, t)

		def get(d: Direction): Option[Cell[T]] = {
			if (cellOnGrid(x + d.dx, y + d.dy)) {
				Some(cell(x + d.dx, y + d.dy))
			} else {
				None
			}
		}

		def onEdge: Boolean = cellOnEdge(x, y)

		/**
		 *
		 * @return The set of directions that are still on the grid
		 */
		def validDirections: DirectionSet = {
			// Don't bother filtering if there's nothing to filter
			if (!onEdge) {
				DirectionSet.All
			} else {
				DirectionSet.All.filter(d => cellOnGrid(x + d.dx, y + d.dy))
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
		val width = lines.maxBy(_.length).length
		val retVal = new Grid[T](width, height)
		for ((line, y) <- lines.zipWithIndex) {
			for((char, x) <- line.zipWithIndex) {
				retVal(x, y) = transform(char)
			}
		}
		retVal
	}

	implicit def renderBoolean(b: Boolean): Char = {
		if (b) '#' else ' '
	}
}
