package common

import scala.collection.mutable

/**
 * Basic graph data structure storing custom data in both the vertices and the
 * edges. The methods on the Graph itself are set up to generally use the vertex
 * objects directly.
 *
 * @tparam V The vertex type.
 * @tparam E The edge type.
 */
class Graph[V, E] extends Iterable[GraphVertex[V, E]] {
	private val vertexIndex = mutable.LinkedHashMap.empty[V, GraphVertexImpl]

	def add(vertex: V): Boolean = {
		if (vertexIndex.contains(vertex)) {
			false
		} else {
			vertexIndex.addOne(vertex, new GraphVertexImpl(vertex))
			true
		}
	}

	def get(vertex: V): Option[GraphVertex[V, E]] = vertexIndex.get(vertex)

	def contains(vertex: V): Boolean = vertexIndex.contains(vertex)

	def remove(vertex: V): Boolean = {
		val maybeVertex = vertexIndex.remove(vertex)
		if (maybeVertex.isDefined) {
			val vertex = maybeVertex.get
			vertex.removeAllEdges()
			true
		} else {
			false
		}
	}

	def vertices: Set[V] = vertexIndex.keySet.toSet

	def degreeOf(vertex: V): Int = {
		vertexIndex.getOrElse(vertex, throw new NoSuchElementException(vertex + " is not an element")).degree
	}

	def neighborsOf(vertex: V): Set[V] = vertexIndex.getOrElse(vertex, throw new NoSuchElementException(vertex + " is not an element")).edgeIndex.keySet.toSet

	def iterator: Iterator[GraphVertex[V, E]] = vertexIndex.valuesIterator

	def dfsFrom(vertex: V): Iterator[V] = new Search(vertexIndex.getOrElse(vertex, throw throw new NoSuchElementException(vertex + " is not an element")), true)

	def bfsFrom(vertex: V): Iterator[V] = new Search(vertexIndex.getOrElse(vertex, throw throw new NoSuchElementException(vertex + " is not an element")), false)

	private class GraphVertexImpl(val value: V) extends GraphVertex[V, E] {
		private[Graph] val edgeIndex = mutable.LinkedHashMap.empty[V, GraphEdgeImpl]
		vertexIndex(value) = this

		def hasEdgeTo(target: V): Boolean = edgeIndex.contains(target)

		def getEdgeTo(target: V): Option[GraphEdgeImpl] = edgeIndex.get(target)

		def addEdgeTo(target: V, value: E): Boolean = {
			if (edgeIndex.contains(target)) {
				false
			} else {
				val otherVertex = vertexIndex.getOrElse(target, throw new IllegalArgumentException(target + " is not a valid Vertex!"))
				val newEdge = new GraphEdgeImpl(this, otherVertex, value)
				edgeIndex.put(target, newEdge)
				otherVertex.edgeIndex.put(this.value, newEdge)
				true
			}
		}

		def removeEdgeTo(target: V): Boolean = {
			val maybeEdge = edgeIndex.get(target)
			if (maybeEdge.isDefined) {
				maybeEdge.get.remove()
			} else {
				false
			}
		}

		def degree: Int = edgeIndex.size

		def isAdjacentTo(target: V): Boolean = edgeIndex.contains(target)

		def adjacentVertices: Set[GraphVertex[V, E]] = {
			edgeIndex.values.map(_.other(this)).toSet
		}

		def incidentEdges: Set[GraphEdge[V, E]] = {
			edgeIndex.values.toSet
		}

		def bfs(): Iterator[V] = new Search(this, false)
		def dfs(): Iterator[V] = new Search(this, true)

		private[Graph] def removeAllEdges(): Unit = {
			for (e <- edgeIndex.keys.toList) {
				removeEdgeTo(e)
			}
		}
	}

	private class GraphEdgeImpl(val v0: GraphVertexImpl, val v1: GraphVertexImpl, val value: E) extends GraphEdge[V, E] {
		def remove(): Boolean = {
			v0.edgeIndex.remove(v1.value)
			v1.edgeIndex.remove(v0.value)
			true
		}

		private[Graph] def other(vertex: GraphVertexImpl): GraphVertexImpl = {
			if (v0 == vertex) v1 else v0
		}
	}

	private class Search(source: GraphVertex[V, E], dfs: Boolean) extends Iterator[V] {
		private val deque = mutable.ArrayDeque.from(Some(source))
		private val visited = mutable.HashSet.empty[GraphVertex[V, E]]

		def hasNext: Boolean = deque.nonEmpty

		def next(): V = {
			if (deque.isEmpty) {
				throw new IllegalStateException("No more vertices in graph search.")
			}
			val retVal = if (dfs)
				deque.removeLast()
			else
				deque.removeHead()
			visited.add(retVal)
			deque.appendAll(retVal.adjacentVertices)
			if (dfs) {
				deque.removeLastWhile(visited)
			} else {
				deque.removeHeadWhile(visited)
			}
			retVal.value
		}
	}
}

/**
 * A vertex in a graph containing information associated with it.
 *
 * @tparam V The vertex type.
 * @tparam E The edge type.
 */
trait GraphVertex[V, E] {
	def value: V

	def hasEdgeTo(target: V): Boolean
	def getEdgeTo(target: V): Option[GraphEdge[V, E]]
	def addEdgeTo(target: V, value: E): Boolean
	def removeEdgeTo(target: V): Boolean

	def degree: Int
	def isAdjacentTo(target: V): Boolean
	def adjacentVertices: Set[GraphVertex[V, E]]
	def incidentEdges: Set[GraphEdge[V, E]]

	def bfs(): Iterator[V]
	def dfs(): Iterator[V]
}

/**
 * An edge in a graph containing data associated with the edge such as a weight.
 *
 * @tparam V The vertex type.
 * @tparam E The edge type.
 */
trait GraphEdge[V, E] {
	def v0: GraphVertex[V, E]
	def v1: GraphVertex[V, E]
	def value: E

	def remove(): Boolean
}
