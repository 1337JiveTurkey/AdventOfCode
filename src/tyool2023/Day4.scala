package tyool2023

import scala.util.matching.Regex

object Day4 extends Main {
	def main(args: Array[String]): Unit = {
		star2()
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
				Card(cardNumber.toInt - 1, dividedNumbers(winningNumbers).toSet, dividedNumbers(gameNumbers).toSet)
		}
		for (card <- cards) {
			println(card.numberOfMatches)
		}
		println(cards.map(_.value).sum)
	}

	def star2(): Unit = {
		val lines = fileLines("Day4.txt")

		val cards = lines map {
			case cardPattern(cardNumber, winningNumbers, gameNumbers) =>
				Card(cardNumber.toInt - 1, dividedNumbers(winningNumbers).toSet, dividedNumbers(gameNumbers).toSet)
		}
		val cardCounts = new Array[Int](cards.length)
		var totalCopies = 0
		for (card <- cards) {
			val n = card.cardNumber
			// Add the original card to the counts
			cardCounts(n) += 1
			val copies = cardCounts(n)
			for (i <- 1 to card.numberOfMatches) {
				cardCounts(n + i) += copies
			}
			totalCopies += copies
		}
		println(totalCopies)
	}

	/**
	 * A scratch-off card with two sets of numbers. The card is part of a game
	 * where the numbers on the right are winning if they match any of the numbers
	 * on the left hand side.
	 *
	 * @param cardNumber The number of the card in the file.
	 * @param winningNumbers The set of winning numbers on the card itself.
	 * @param gameNumbers The set of selected numbers for the game.
	 */
	case class Card(cardNumber: Int, winningNumbers: Set[Int], gameNumbers: Set[Int]) {
		lazy val matchingNumbers: Set[Int] = winningNumbers intersect gameNumbers
		lazy val numberOfMatches: Int = matchingNumbers.size

		lazy val value: Int = {
			if (numberOfMatches == 0) {
				0
			}
			else {
				1 << (numberOfMatches - 1)
			}
		}
	}
}
