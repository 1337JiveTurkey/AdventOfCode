package tyool2018

import scala.collection.mutable

object Day8 extends Main {
	def main(args: Array[String]): Unit = {
		star2()
	}

	def star1(): Unit = {
		val line = fileLine("Day8.txt").split(" ").flatMap(_.toIntOption)
		val node = buildNode(line.iterator)
		println(node.weight)
	}

	def star2(): Unit = {
		val line = fileLine("Day8.txt").split(" ").flatMap(_.toIntOption)
		val node = buildNode(line.iterator)
		println(node.value)
	}

	def buildNode(line: Iterator[Int]): Node = {
		val childCount = line.next()
		val metadataCount = line.next()
		val children = mutable.ArrayBuffer.empty[Node]
		val metadata = mutable.ArrayBuffer.empty[Int]
		val retVal = Node(children, metadata)
		for (i <- 0 until childCount) {
			children.addOne(buildNode(line))
		}
		for (i <- 0 until metadataCount) {
			metadata.addOne(line.next())
		}
		retVal
	}

	case class Node(children: mutable.IndexedSeq[Node], metadata: mutable.IndexedSeq[Int]) {
		lazy val weight: Int = children.map(_.weight).sum + metadata.sum
		lazy val value: Int = {
			if (children.nonEmpty) {
				var sum = 0
				for (i <- metadata) {
					if (i <= children.length) {
						sum += children(i - 1).value
					}
				}
				sum
			} else {
				metadata.sum
			}
		}
	}
}
