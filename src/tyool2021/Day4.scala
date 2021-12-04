package tyool2021

import scala.collection.immutable.BitSet
import scala.collection.mutable
import scala.util.matching.Regex


object Day4 extends Main {
	def main(args: Array[String]): Unit = {
		star8()
	}

	def star7(): Unit = {
		val lines = fileLines("Star7.txt")
		val calls = lines.head.split(",").map(_.toInt)
		val cardLines = lines.tail.grouped(6)
		val cards =
			for (cardText <- cardLines)
				yield Card(cardText.toList.tail)
		val cardList = cards.toList
		for (n <- 5 to calls.length) {
			// For computations if we win
			val lastCall = calls(n - 1)
			val callSet = BitSet.fromSpecific(calls.take(n))
			val winner = cardList.find(_.isWinning(callSet))
			if (winner.isDefined) {
				val w = winner.get
				println(w.uncalledNumbers(callSet).sum * lastCall)
				return
			}
		}
	}

	def star8(): Unit = {
		val lines = fileLines("Star7.txt")
		val calls = lines.head.split(",").map(_.toInt)
		val cardLines = lines.tail.grouped(6)
		val cards =
			for (cardText <- cardLines)
				yield Card(cardText.toList.tail)
		var losers = cards.toBuffer
		var winner: Card = null
		for (n <- 5 to calls.length) {
			// For computations if we win
			val lastCall = calls(n - 1)
			val callSet = BitSet.fromSpecific(calls.take(n))
			losers = losers.filter(!_.isWinning(callSet)).toBuffer
			if (losers.size == 1) {
				winner = losers.head
			} else if (losers.isEmpty) {
				println(winner.uncalledNumbers(callSet).sum * lastCall)
				return
			}
		}
	}
}

class Card(rows: List[List[Int]]) {
	val allValues: BitSet = BitSet.fromSpecific(rows.flatten)
	val winners: mutable.Buffer[BitSet] = mutable.Buffer[BitSet]()
	for (list <- rows ++ rows.transpose) {
		winners.append(BitSet.fromSpecific(list))
	}

	def isWinning(calls: BitSet): Boolean = {
		for (candidate <- winners) {
			if ((calls & candidate).size == 5) {
				return true
			}
		}
		false
	}

	def calledNumbers(calls: BitSet): BitSet = {
		calls & allValues
	}
	def uncalledNumbers(calls: BitSet): BitSet = {
		allValues diff calls
	}
}

object Card extends (List[String] => Card) {
	private val row: Regex = """\W?(\d{1,2})\W{1,2}(\d{1,2})\W{1,2}(\d{1,2})\W{1,2}(\d{1,2})\W{1,2}(\d{1,2})""".r

	private def parseRow(r: String): List[Int] = {
		r match {
			case row(c1, c2, c3, c4, c5) => List(c1.toInt, c2.toInt, c3.toInt, c4.toInt, c5.toInt)
		}
	}
	override def apply(strings: List[String]): Card = {
		new Card(strings map parseRow)
	}
}