package tyool2016

import scala.collection.mutable

object Day5 extends Main {
	def main(args: Array[String]): Unit = {
		star2()
	}

	def star1(): Unit = {
		val prefix = "uqwqemis"
		val hashes = for (i <- (0 to Int.MaxValue).iterator) yield md5(prefix + i)
		val filtered = hashes.filter(_.startsWith("00000"))
		for (i <- filtered.take(8)) {
			println(i)
		}
	}

	def star2(): Unit = {
		val prefix = "uqwqemis"
		val hashes = for (i <- (0 to Int.MaxValue).iterator) yield md5(prefix + i)
		val filtered = hashes.filter(_.startsWith("00000"))
		val password = mutable.ArrayBuffer.from("********")
		for (i <- filtered) {
			val positionChar = i.drop(5).head
			if (positionChar.isDigit) {
				val position = positionChar.toString.toInt
				if (position < 8 && password(position) == '*') {
					val passwordChar = i.drop(6).head
					password(position) = passwordChar
					println(s"Password char $position is $passwordChar")
				}
			}
			if (password.forall(_.isDigit)) {
				println(password)
				return
			}
		}
	}
}
