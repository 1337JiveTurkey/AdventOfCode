package common

import scala.collection.mutable

/**
 * A mutable Tree data structure.
 *
 * @tparam K The type of the tree node identifier.
 * @tparam V The type of the tree node value.
 */
class Tree[K, V] {
	private[this] var root: Option[TreeNodeImpl] = None
	private[this] val index = mutable.HashMap.empty[K, TreeNodeImpl]

	private case class TreeNodeImpl(id: K) extends TreeNode[K, V] {
		var value: V = null.asInstanceOf[V]
		var parentID: Option[K] = None
		var childIDs: List[K] = List.empty
		def parent: Option[TreeNode[K, V]] = parentID.map(index)
		def children: List[TreeNode[K, V]] = childIDs.map(index)
	}
}

object Tree {

}

trait TreeNode[K, V] {
	def id: K
	def value: V
	def value_=(newValue: V): Unit
	def parent: Option[TreeNode[K, V]]
	def children: List[TreeNode[K, V]]

	def ancestors: List[TreeNode[K, V]] = {
		val pathBuilder = List.newBuilder[TreeNode[K, V]]
		var node = Some(this)
		while (node.isDefined) {

		}


		pathBuilder.result()
	}
}
