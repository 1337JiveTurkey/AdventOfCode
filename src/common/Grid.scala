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
class Grid[T: ClassTag](val width: Int, val height: Int) extends Iterable[T] {
	private val contents = new Array[T](width * height)

	def apply(x: Int, y: Int): T = contents(x + width * y)
	def update(x: Int, y: Int, t: T): Unit = contents(x + width * y) = t

	override def iterator: Iterator[T] = contents.iterator
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
	def apply[T: ClassTag](lines: Seq[String])(transform: (Char) => T): Grid[T] = {
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
	def withCoordinates[T: ClassTag](lines: Seq[String])(transform: (Int, Int, Char) => T): Grid[T] = {
		val height = lines.length
		val width = lines.head.length
		val retVal = new Grid[T](width, height)
		for ((line, y) <- lines.zipWithIndex) {
			for((char, x) <- line.zipWithIndex) {
				retVal(x, y) = transform(x, y, char)
			}
		}
		retVal
	}
}