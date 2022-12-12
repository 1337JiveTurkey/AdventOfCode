package tyool2022

import scala.collection.mutable
import scala.util.matching.Regex

object Day7 extends Main {
	def main(args: Array[String]): Unit = {
		star2()
	}

	val CDPattern: Regex = """\$ cd (.+)""".r
	val LSPattern: Regex = """\$ ls""".r
	val FileSizePattern: Regex = """(\d+) (.+)""".r
	val DirNamePattern: Regex = """dir (.+)""".r

	def star1(): Unit = {
		val lines = fileLines("Day7.txt")
		val root = buildTree(lines)
//		println(root)
		for (dir <- root.dirStream.filter(_.totalSize <= 100000)) {
			println(dir.dirName)
		}
		println(root.dirStream.filter(_.totalSize <= 100000).map(_.totalSize).sum)
	}

	def star2(): Unit = {
		val lines = fileLines("Day7.txt")
		val root = buildTree(lines)
		val capacity = 70_000_000
		val remaining = capacity - root.totalSize
		val needed_to_install = 30_000_000
		val need_to_delete = needed_to_install - remaining
		println(need_to_delete)
		val candidates = root.dirStream.filter(_.totalSize > need_to_delete).toIndexedSeq.sortBy(_.totalSize)
		for (candidate <- candidates) {
			println(candidate)
		}
	}

	def buildTree(lines: Iterable[String]): Directory = {
		val root = new Directory("/", null)
		var pwd = root
		for (line <- lines) line match {
			case CDPattern("/") => pwd = root
			case CDPattern("..") => pwd = pwd.parent
			case CDPattern(dirName) => pwd = pwd.dirs(dirName)
			case LSPattern() => // Ignore, following entries are parsed independently of this
			case FileSizePattern(size, name) => pwd.files = pwd.files + (name -> size.toInt)
			case DirNamePattern(dirName) => pwd.dirs = pwd.dirs + (dirName -> new Directory(dirName, pwd))
		}
		root
	}

	class Directory(val dirName: String, val parent: Directory) {
		var dirs: Map[String, Directory] = Map.empty[String, Directory]
		var files: Map[String, Int] = Map.empty[String, Int]
		// Yeah it's a val depending on vars but we're never touching the vars after buildTree()
		lazy val totalSize: Int = dirs.values.map(_.totalSize).sum + files.values.sum

		lazy val dirStream: LazyList[Directory] = LazyList(this).appendedAll(dirs.values.flatMap(_.dirStream))

		override def toString: String = {
			dirName + " (" + totalSize + ")"
		}

		def toTree: String = {
			val sb = new mutable.StringBuilder(toString).append("\n")
			val prefix = " "
			for (dir <- dirs.values) {
				dir.printTree(sb, prefix)
			}
			sb.toString()
		}

		private def printTree(sb: mutable.StringBuilder, prefix: String): Unit = {
			sb.append(prefix).append(toString).append("\n")
			for (dir <- dirs.values) {
				dir.printTree(sb, prefix + " ")
			}
		}
	}
}
