package tyool2015

import scala.util.matching.Regex

object Day14 extends Main {
	val reindeerPattern: Regex = """(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds.""".r

	private val totalTime: Int = 2503

	def main(args: Array[String]): Unit = {
		star2()
	}

	def star1(): Unit = {
		val lines = fileLines("Day14.txt")
		val reindeer = lines map {
			case reindeerPattern(name, speed, onTime, offTime) =>
				Reindeer(name, speed.toInt, onTime.toInt, offTime.toInt)
		}
		for (r <- reindeer) {
			val cycles = totalTime / r.cycleTime
			val leftoverTime = totalTime % r.cycleTime
			val lastOnTime = Math.min(r.onTime, leftoverTime)
			val distance = (cycles * r.onTime + lastOnTime) * r.speed
			println(r.name + ": " + distance)
		}
	}

	def star2(): Unit = {
		val lines = fileLines("Day14.txt")
		val reindeer = lines map {
			case reindeerPattern(name, speed, onTime, offTime) =>
				Reindeer(name, speed.toInt, onTime.toInt, offTime.toInt)
		}
		for (t <- 1 to totalTime) {
			var bestDistance = 0
			for (r <- reindeer) {
				val distance = r.simulate()
				if (bestDistance < distance) {
					bestDistance = distance
				}
			}
			for (r <- reindeer) {
				if (r.distance == bestDistance) {
					r.points += 1
				}
			}
		}
		for (r <- reindeer) {
			println(r.name + ": " + r.distance + " " + r.points)
		}
	}

	case class Reindeer(name: String, speed: Int, onTime: Int, offTime: Int) {
		val cycleTime: Int = onTime + offTime

		var distance = 0
		var points = 0

		var onTimeRemaining: Int = onTime
		var offTimeRemaining: Int = offTime

		def simulate(): Int = {
			if (onTimeRemaining > 0) {
				onTimeRemaining -= 1
				distance += speed
			}
			else if (offTimeRemaining > 0) {
				offTimeRemaining -= 1
				if (offTimeRemaining == 0) {
					onTimeRemaining = onTime
					offTimeRemaining = offTime
				}
			}
			distance
		}
	}
}
