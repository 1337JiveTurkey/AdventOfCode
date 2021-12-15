package common

import scala.reflect.ClassTag

/**
 * 2D grid for various operations thereon.
 *
 * @param width Width of the grid
 * @param height Height of the grid
 * @param classTag$T$0 Type of the underlying storage array
 * @tparam T Type of the grid
 */
class Grid[T: ClassTag](val width: Int, val height: Int) {
	private val contents = new Array[T](width * height)

	def apply(x: Int, y: Int): T = contents(x + width * y)
	def update(x: Int, y: Int, t: T): Unit = contents(x + width * y) = t
}
