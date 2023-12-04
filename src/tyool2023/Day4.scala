package tyool2023

import scala.util.matching.Regex

object Day4 extends Main {
	def main(args: Array[String]): Unit = {
		star1()
	}

	/**
	 * Pattern that matches the card number followed by a colon, then a series of
	 * numbers followed by a pipe followed by another series of numbers. The extra
	 * crap is just trimming excess whitespace.
	 */
	val cardPattern: Regex = """Card\s+(\d+):\s+([\s\d]+?)\s+\|\s+([\s\d]+)""".r

	def star1(): Unit = {
		val lines = fileLines("Day4.txt")

		val cards = lines map {
			case cardPattern(cardNumber, winningNumbers, gameNumbers) =>
				Card(cardNumber.toInt, whitespaceDividedNumbers(winningNumbers).toSet, whitespaceDividedNumbers(gameNumbers).toSet)
		}
		for (card <- cards) {
			println(card.value)
		}
		println(cards.map(_.value).sum)
	}

	case class Card(cardNumber: Int, winningNumbers: Set[Int], gameNumbers: Set[Int]) {
		lazy val matchingNumbers: Set[Int] = winningNumbers intersect gameNumbers
		lazy val value: Int = {
			if (matchingNumbers.isEmpty) {
				0
			}
			else {
				1 << (matchingNumbers.size - 1)
			}
		}
	}
}
