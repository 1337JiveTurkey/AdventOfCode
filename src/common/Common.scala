package common

import java.security.MessageDigest
import scala.collection.mutable
import scala.io.Source

class Common(dirPrefix: String) {
	def fileLines(filename: String): IndexedSeq[String] = {
		val file = Source.fromFile(dirPrefix + filename)
		try {
			// Fully parses the file into memory with this step so we can safely close
			file.getLines().toIndexedSeq
		} finally {
			file.close()
		}
	}

	def fileLine(filename: String): String = {
		val lines = fileLines(filename)
		assert(lines.length == 1)
		lines.head
	}

	def fileNumbers(filename: String): IndexedSeq[Int] = {
		val line = fileLine(filename)
		line.split(",").flatMap(_.toIntOption)
	}

	def whitespaceDividedNumbers(line: String): IndexedSeq[Int] = {
		line.split("\\s+").flatMap(_.toIntOption)
	}

	// Splits a string into a list of 2 character substrings
	def pairwiseStrings(string: String): Iterator[String] = {
		string.sliding(2)
	}

	def pairwise[T](list: Iterator[T]): Iterator[(T, T)] = {
		list.sliding(2).map(pair => (pair.head, pair.tail.head))
	}

	// Like pairwise but considering three adjacent elements at a time
	def tripwise[T](list: Iterator[T]): Iterator[(T, T, T)] = {
		list.sliding(3).map(pair => (pair.head, pair(1), pair(2)))
	}

	// Same as the above two but four adjacent elements
	def quadwise[T](list: Iterator[T]): Iterator[(T, T, T, T)] = {
		list.sliding(4).map(pair => (pair.head, pair(1), pair(2), pair(3)))
	}

	// Pairs split into separate lists, even-odd
	def halves[T](list: Seq[T]): (Seq[T], Seq[T]) = {
		list.grouped(2).map(pair => (pair.head, pair.tail.head)).toList.unzip
	}

	// Distinct pairs of elements from the list
	def allPairs[T](list: IndexedSeq[T]): IndexedSeq[(T, T)] = {
		val retVal = IndexedSeq.newBuilder[(T, T)]
		for (x <- list.indices) {
			for (y <- list.indices) {
				if (x != y) {
					retVal.addOne((list(x), list(y)))
				}
			}
		}
		retVal.result()
	}

	def repeatForever[T](s: Seq[T]): LazyList[T] = {
		LazyList.continually(s.to(LazyList)).flatten
	}

	private lazy val md = MessageDigest.getInstance("MD5")
	// Generate MD5 hash as a hex string
	def md5(s: String): String = {
		val a = md.digest(s.getBytes)
		val sb = new mutable.StringBuilder(a.length * 2)
		for (b <- a) {
			sb.append(String.format("%02x", b))
		}
		sb.toString
	}

	// Splits an IndexedSeq of strings into stanzas based on blank lines
	def splitOnBlanks(input: IndexedSeq[String]): IndexedSeq[IndexedSeq[String]] = {
		val retVal = IndexedSeq.newBuilder[IndexedSeq[String]]
		val stanza = IndexedSeq.newBuilder[String]
		for (line <- input) {
			if (line.isBlank) {
				retVal += stanza.result()
				stanza.clear()
			}
			else {
				stanza += line
			}
		}
		val remainder = stanza.result()
		if (remainder.nonEmpty) {
			retVal += remainder
		}
		retVal.result()
	}

	def countCharacters(input: Seq[Char]): Map[Char, Int] = {
		input.groupBy(identity).view.mapValues(_.length).toMap
	}

	/**
	 *
	 * @param char The character to turn into a digit.
	 * @return The character as a digit.
	 */
	def toDigit(char: Char): Int = {
		Integer.parseInt(String.valueOf(char))
	}

	/**
	 *
	 * @param n The number to find the digits of.
	 * @return The digits of the given number from least significant to most.
	 */
	def digits(n: Int): List[Int] = {
		if (n == 0) {
			return List(0)
		}
		val lb = List.newBuilder[Int]
		var r = n
		while (r != 0) {
			lb.addOne(r % 10)
			r = r / 10
		}
		lb.result()
	}

	/**
	 *
	 * @param n The number to get the bits of.
	 * @return The numbers of the bits turned on for the given number.
	 */
	def bits(n: Int): List[Int] = {
		var l: List[Int] = Nil
		for (i <- (0 until 32).reverse) {
			val bit: Int = 1 << i
			if ((n & bit) > 0) {
				l = i :: l
			}
		}
		l
	}

}
