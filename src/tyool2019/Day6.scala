package tyool2019

import scala.collection.mutable
import scala.util.matching.Regex

object Day6 extends Main {
	def main(args: Array[String]): Unit = {
		star1()
	}

	val LinePattern: Regex = """(\w+)\)(\w+)""".r
	def star1(): Unit = {
		val lines = fileLines("Day6.txt")
		val map = mutable.HashMap.empty[String, Body]
		for (LinePattern(parentName, childName) <- lines) {
			val parent = map.getOrElseUpdate(parentName, Body(parentName))
			val child = map.getOrElseUpdate(childName, Body(childName))
			parent.children = child :: parent.children
			child.parent = parent
		}
		println(map("COM").updateCounts())
	}

	case class Body(name: String) {
		var parent: Body = null
		var children: List[Body] = List.empty[Body]
		var count: Int = 0

		def updateCounts(): Int = {
			if (parent == null) {
				count = 0
			} else {
				count = parent.count + 1
			}
			count + children.map(_.updateCounts()).sum
		}
	}
}
