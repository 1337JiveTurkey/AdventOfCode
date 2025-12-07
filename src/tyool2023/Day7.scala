package tyool2023

import scala.util.matching.Regex

object Day7 extends Main {
	def main(args: Array[String]): Unit = {
		star1()
	}

	def star1(): Unit = {
		val lines = fileLines("Day7.txt")
		val hands = lines map {
			case handPattern(cards, wager) => Star1Hand(cards, wager.toInt)
		}
		for (hand <- hands) {
			println(hand.star1Order)
		}
	}

	val handPattern: Regex = """([AKQJT2-9]{5}) (\d+)""".r
	val translateStar1Chars: Map[Char, Char] = Map.from("AKQJT98765432" zip "abcdefghijklm".reverse)
	val translateStar2Chars: Map[Char, Char] = Map.from("AKQT98765432J" zip "abcdefghijklm".reverse)

	case class Star1Hand(cards: String, wager: Int) {
		val characters: Map[Char, Int] = countCharacters(cards)
		val star1Order: String = cards.map(translateStar1Chars)

		val isFiveOfAKind: Boolean = characters.values.exists(_ == 5)
		val isFourOfAKind: Boolean = characters.values.exists(_ == 4)
		val isFullHouse: Boolean = characters.values.exists(_ == 3) && characters.values.exists(_ == 2)
		val isThreeOfAKind: Boolean = characters.values.exists(_ == 3)
		val isTwoPair: Boolean = characters.values.count(_ == 2) == 2
		val isTwoOfAKind: Boolean = characters.values.exists(_ == 2)
	}

}
