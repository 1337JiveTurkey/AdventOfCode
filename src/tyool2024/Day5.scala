package tyool2024

import scala.collection.mutable

object Day5 extends Main {
	def main(args: Array[String]): Unit = {
		star2()
	}

	def star1(): Unit = {
		val lines = splitOnBlanks(fileLines("Day5.txt"))
		val pageOrderLines = lines(0)
		val pageListLines = lines(1)

		val pageOrders = generatePageOrders(pageOrderLines)
		val pageLists = pageListLines.map(dividedNumbers)

		var total = 0

		for (pageList <- pageLists) {
			val update = new Update(pageList, pageOrders)
			if (update.valid) {
				total += update.middlePage
			}
		}
		println(total)
	}

	def star2(): Unit = {
		val lines = splitOnBlanks(fileLines("Day5.txt"))
		val pageOrderLines = lines(0)
		val pageListLines = lines(1)

		val pageOrders = generatePageOrders(pageOrderLines)
		val pageLists = pageListLines.map(dividedNumbers)

		var total = 0

		for (pageList <- pageLists) {
			val update = new Update(pageList, pageOrders)
			if (!update.valid) {
				total += update.generateValidUpdate().middlePage
			}
		}
		println(total)
	}

	/**
	 * Generates a map of pages to sets of pages that must come after them.
	 *
	 * @param pageOrderLines The raw lines to be parsed into page orders.
	 * @return A map of pages that come first to pages that come after them.
	 */
	def generatePageOrders(pageOrderLines: IndexedSeq[String]): Map[Int, Set[Int]] = {
		val mutableMap = mutable.HashMap.empty[Int, mutable.Set[Int]]
		for (line <- pageOrderLines.map(dividedNumbers)) {
			val comesFirst = line(0)
			val comesLast = line(1)
			if (!mutableMap.contains(comesFirst)) {
				val mutableSet = mutable.HashSet.empty[Int]
				mutableMap.put(comesFirst, mutableSet)
			}
			mutableMap(comesFirst).add(comesLast)
		}

		// Now put everything into a nice immutable data structure
		val mb = Map.newBuilder[Int, Set[Int]]
		mb.sizeHint(mutableMap.size)
		for ((key, value) <- mutableMap) {
			mb.addOne((key, Set.from(value)))
		}
		mb.result()
	}

	/**
	 * Update consisting of a series of page numbers and some additional data
	 * telling whether the page order is valid or not. Can generate a valid
	 * sequence of page numbers if the initial page numbers aren't valid.
	 *
	 * @param pageList The list of pages that this update contains
	 * @param pageOrders The orders of the pages for determining validity
	 */
	private class Update(val pageList: IndexedSeq[Int], pageOrders: Map[Int, Set[Int]]) {
		val valid: Boolean = {
			var valid = true
			for (index <- pageList.indices) {
				val page = pageList(index)
				val forbiddenPagesBefore = pageOrders.getOrElse(page, Set.empty)
				// Look at pages before the page in question
				for (pageBefore <- pageList.take(index)) {
					if (forbiddenPagesBefore.contains(pageBefore)) {
						valid = false
					}
				}
			}
			valid
		}
		val middlePage: Int = pageList((pageList.length - 1) / 2)

		def generateValidUpdate(): Update = {
			if (valid) {
				println("Warning: Current update is valid")
				this
			} else {
				// Builder for the new update's page list
				val pl = IndexedSeq.newBuilder[Int]
				// Map needed for the topographic sort
				val topoMap = mutable.HashMap.empty[Int, mutable.HashSet[Int]]
				// The only pages that we need to retain for our pageOrders
				val relevantPages = Set.from(pageList)
				for (page <- pageList) {
					val successors = mutable.HashSet.from(pageOrders.getOrElse(page, Set.empty[Int]))
					val relevantSuccessors = successors.filterInPlace(relevantPages)
					topoMap.put(page, relevantSuccessors)
				}
				while (topoMap.nonEmpty) {
					val nextPages = mutable.HashSet.empty[Int]
					for ((page, successors) <- topoMap) {
						if (successors.isEmpty) {
							nextPages.add(page)
						}
					}
					if (nextPages.isEmpty) {
						throw new IllegalStateException("Pages cannot be sorted")
					} else if (nextPages.size > 1) {
						println(s"${nextPages.size} possible next pages. Nondeterministic!")
					}
					for (page <- nextPages) {
						pl.addOne(page)
						topoMap.remove(page)
						for (successors <- topoMap.values) {
							successors.remove(page)
						}
					}
				}
				// Valid update has the pages in the opposite order we built it
				new Update(pl.result().reverse, pageOrders)
			}
		}
	}
}
