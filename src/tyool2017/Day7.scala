package tyool2017

import scala.collection.immutable.TreeSet
import scala.util.matching.Regex

object Day7 extends Main {
	def main(args: Array[String]): Unit = {
		star2()
	}

	val ShortLine: Regex = """(\w+) \((\d+)\)""".r
	val LongLine: Regex = """(\w+) \((\d+)\) -> (.+)""".r

	def star1(): Unit = {
		val lines = fileLines("Day7.txt")
		val root = buildProcessTree(lines)
		println(root)
	}

	def star2(): Unit = {
		val lines = fileLines("Day7.txt")
		val root = buildProcessTree(lines)
		println(root.findWrongWeight())
	}

	def buildProcessTree(lines: IndexedSeq[String]): Process = {
		val mb = Map.newBuilder[String, Process]
		val sb = TreeSet.newBuilder[Int]
		for (line <- lines) {
			line match {
				case ShortLine(name, weightString) => {
					mb.addOne(name, Process(name, weightString.toInt, IndexedSeq.empty[String]))
					sb.addOne(0)
				}
				case LongLine(name, weightString, childrenString) => {
					val childrenNames = childrenString.split(", ").toIndexedSeq
					mb.addOne(name, Process(name, weightString.toInt, childrenNames))
					sb.addOne(childrenNames.length)
				}
			}
		}
		println("Children numbers = " + sb.result())
		val processes = mb.result()
		for (process <- processes.values) {
			process.linkChildren(processes)
		}
		processes.values.head.root
	}

	case class Process(name: String, weight: Int, childrenNames: IndexedSeq[String]) {
		var parent: Process = null
		var children: IndexedSeq[Process] = null

		def linkChildren(processes: Map[String, Process]): Unit = {
			val isb = IndexedSeq.newBuilder[Process]
			for (childName <- childrenNames) {
				val child = processes(childName)
				child.parent = this
				isb.addOne(child)
			}
			children = isb.result()
		}

		def root: Process = {
			if (parent == null)
				this
			else
				parent.root
		}

		def totalWeight: Int = {
			weight + childrenWeight
		}

		def childrenWeight: Int = {
			children.foldLeft(0)(_ + _.totalWeight)
		}

		def findWrongWeight(): Process = {
			val weights = children.groupBy(_.totalWeight)
			if (weights.size < 2) {
				this
			} else {
				assert(weights.size == 2)
				val (weight1, list1) = weights.head
				val (weight2, list2) = weights.tail.head
				println(weight1 + ": " + list1 + ", "+ weight2 + ": " + list2)
				if (list1.size == 1) {
					list1.head.findWrongWeight()
				} else {
					list2.head.findWrongWeight()
				}
			}
		}
	}
}
