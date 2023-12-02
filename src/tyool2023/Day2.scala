package tyool2023

import scala.util.matching.Regex

object Day2 extends Main {
	val gamePattern: Regex = """Game (\d+): (.+)""".r
	val drawPattern: Regex = """(\d+) (red|green|blue)""".r

	def main(args: Array[String]): Unit = {
		star2()
	}

	def star1(): Unit = {
		val lines = fileLines("Day2.txt")
		val games = lines map parseGame
		val possibleGames = games.filter(_.compatibleWith(12, 13, 14))
		println(possibleGames.map(_.gameNumber).sum)
	}

	def star2(): Unit = {
		val lines = fileLines("Day2.txt")
		val games = lines map parseGame
		println(games.map(_.power).sum)
	}

	def parseGame(gameLine: String): Game = gameLine match {
		case gamePattern(gameNumber, draws) => {
			val drawArray = draws.split("; ") map parseDraw
			Game(gameNumber.toInt, drawArray.toIndexedSeq)
		}
	}

	def parseDraw(drawClause: String): Draw = {
		var red = 0
		var green = 0
		var blue = 0

		for (colorDraw <- drawClause.split(", ")) {
			colorDraw match {
				case drawPattern(count, "red") => red = count.toInt
				case drawPattern(count, "green") => green = count.toInt
				case drawPattern(count, "blue") => blue = count.toInt
			}
		}
		Draw(red, green, blue)
	}

	case class Game(gameNumber: Int, draws: IndexedSeq[Draw]) {
		lazy val maxRed: Int = draws.maxBy(_.red).red
		lazy val maxGreen: Int = draws.maxBy(_.green).green
		lazy val maxBlue: Int = draws.maxBy(_.blue).blue

		def compatibleWith(red: Int, green: Int, blue: Int): Boolean = {
			red >= maxRed && green >= maxGreen && blue >= maxBlue
		}

		lazy val power: Int = maxRed * maxGreen * maxBlue
	}

	case class Draw(red: Int, green: Int, blue: Int)
}
