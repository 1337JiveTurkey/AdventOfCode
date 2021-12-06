package common

import java.security.MessageDigest
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
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
		return lines.head
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

	def halves[T](list: Seq[T]): (Seq[T], Seq[T]) = {
		list.grouped(2).map(pair => (pair.head, pair.tail.head)).toList.unzip
	}

	private lazy val md = MessageDigest.getInstance("MD5")
	// Generate MD5 hash as a hex string
	def md5(s: String): String = {
		val a = md.digest(s.getBytes)
		val sb = new StringBuilder(a.length * 2)
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
}
