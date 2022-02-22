package tyool2016

import scala.collection.mutable
import scala.util.matching.Regex

object Day4 extends Main {
	def main(args: Array[String]): Unit = {
		star2()
	}

	val lineRegex: Regex = """([a-z-]+)(\d+)\[([a-z]{5})]""".r
	case class Room(roomName: String, sectorNumber: Int, checksum: String) {
		val valid: Boolean = {
			// Remove the hyphens that we just mashed into the first part
			val counts = countCharacters(roomName).removed('-')
			val reversed = new mutable.TreeMap[Int, String]()
			for ((char, count) <- counts) {
				val existing = reversed.getOrElse(count, "")
				reversed.put(count, (existing + char).sorted)
			}
			val largeChecksum = reversed.foldRight("")((entry, text) => text + entry._2)
			largeChecksum.startsWith(checksum)
		}

		val decrypted: String = {
			val constant = "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz"
			val shift = sectorNumber % 26
			val sb = new StringBuilder
			for (char <- roomName) {
				if (char < 96) {
					sb.append(char)
				}
				else {
					sb.append((((char + shift - 96) % 26) + 96).toChar)
				}
			}
			sb.result()
		}
	}

	def star1(): Unit = {
		val rooms = fileLines("Day4.txt") map {
			case lineRegex(roomName, sectorNumber, checksum) => Room(roomName, sectorNumber.toInt, checksum)
		}
		val validRooms = rooms.filter(_.valid)
		var total = 0
		for (room <- validRooms) {
			println(room)
			total = total + room.sectorNumber
		}
		println(total)
	}

	def star2(): Unit = {
		val rooms = fileLines("Day4.txt") map {
			case lineRegex(roomName, sectorNumber, checksum) => Room(roomName, sectorNumber.toInt, checksum)
		}
		val validRooms = rooms.filter(_.valid)
		for (room <- validRooms) {
			println(room.decrypted + room.sectorNumber)
		}
	}
}
