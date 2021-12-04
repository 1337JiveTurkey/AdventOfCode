package tyool2020
import scala.collection.mutable

object Day3 extends Main {
	val lines: IndexedSeq[String] = fileLines("Day3.txt")

	def main(args: Array[String]): Unit = {
		star6()
	}

	def hasTree(x: Int, y: Int): Boolean = {
		if (y < 0 || y >= lines.length) {
			false
		} else {
			lines(y)(x % 31) == '#'
		}
	}
	val ugh: ((Int, Int)) => Boolean = (hasTree _) .tupled

	def getPath(dx: Int, dy: Int): mutable.Buffer[(Int, Int)] = {
		val path = for (y <- lines.indices) yield (dx * y, dy * y)
		path.toBuffer
	}

	def star5(): Unit = {
		val path = getPath(3, 1)
		println(path.count(ugh))
	}

	def star6(): Unit = {
		val path1 = getPath(1, 1).count(ugh)
		val path2 = getPath(3, 1).count(ugh)
		val path3 = getPath(5, 1).count(ugh)
		val path4 = getPath(7, 1).count(ugh)
		val path5 = getPath(1, 2).count(ugh)
		println(1L * path1 * path2 * path3 * path4 * path5)
	}
}
