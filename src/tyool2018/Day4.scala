package tyool2018

import scala.util.matching.Regex

object Day4 extends Main {
	def main(args: Array[String]): Unit = {
		star2()
	}

	val LinePattern: Regex = """\[1518-(\d+)-(\d+) (\d+):(\d+)\] (.+)""".r

	def star1(): Unit = {
		val lines = fileLines("Day4Sorted.txt")

		var guards = Map.empty[String, Guard]
		var guard: Guard = null
		var startTime = 0
		for (LinePattern(month, day, hour, minute, note) <- lines) {
			if (note.contains("wakes up")) {
				guard.addMinutes(startTime until minute.toInt)
			}
			else if (note.contains("falls asleep")) {
				startTime = minute.toInt
			}
			else {
				guard = guards.getOrElse(note, null)
				if (guard == null) {
					guard = Guard(note)
					guards = guards.+((note, guard))
				}
			}
		}
		val guardsList = Array.from(guards.values).sortBy(-_.totalMinutes)
		println(guardsList.head + " " + guardsList.head.maxMinute)
	}

	def star2(): Unit = {
		val lines = fileLines("Day4Sorted.txt")

		var guards = Map.empty[String, Guard]
		var guard: Guard = null
		var startTime = 0
		for (LinePattern(month, day, hour, minute, note) <- lines) {
			if (note.contains("wakes up")) {
				guard.addMinutes(startTime until minute.toInt)
			}
			else if (note.contains("falls asleep")) {
				startTime = minute.toInt
			}
			else {
				guard = guards.getOrElse(note, null)
				if (guard == null) {
					guard = Guard(note)
					guards = guards.+((note, guard))
				}
			}
		}
		val guardsList = Array.from(guards.values).sortBy(-_.maxMinutes)
		println(guardsList.head + " " + guardsList.head.maxMinute)
	}

	case class Guard(id: String) extends Comparable[Guard] {
		val minutes: Array[Int] = new Array[Int](60)
		var totalMinutes: Int = 0

		def addMinutes(range: Range): Unit = {
			for (i <- range) {
				minutes(i) = minutes(i) + 1
				totalMinutes += 1
			}
		}

		def maxMinutes: Int = {
			minutes.max
		}

		def maxMinute: Int = {
			minutes.indexOf(maxMinutes)
		}

		override def compareTo(o: Guard): Int = o.totalMinutes.compareTo(totalMinutes)
	}
}
