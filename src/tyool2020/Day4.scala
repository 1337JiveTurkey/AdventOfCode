package tyool2020

import scala.collection.mutable
import scala.util.matching.Regex

object Day4 extends Main {
	val lines: IndexedSeq[String] = fileLines("Day4.txt")

	def main(args: Array[String]): Unit = {
		star7()
	}

	val fieldRegex: Regex = """([a-z]{3}):(\S+)""".r

	def parsePassport(lines: IndexedSeq[String]): Map[String, String] = {
		val passport = Map.newBuilder[String, String]
		for (line <- lines) {
			for (field <- fieldRegex.findAllMatchIn(line)) {
				passport.addOne((field.group(1), field.group(2)))
			}
		}
		passport.result()
	}

	def parseFile(lines: IndexedSeq[String]): List[Map[String, String]] = {
		val paragraphs = splitOnBlanks(lines)
		val pBuilder = List.newBuilder[Map[String, String]]
		pBuilder.sizeHint(paragraphs.length)
		for (paragraph <- paragraphs) {
			pBuilder += parsePassport(paragraph)
		}
		pBuilder.result()
	}

	def fieldsExist(passport: Map[String, String]): Boolean = {
		val keys = passport.keySet
		keys.contains("byr") && keys.contains("iyr") && keys.contains("eyr") &&
			keys.contains("hgt") && keys.contains("hcl") && keys.contains("ecl") &&
			keys.contains("pid")
	}

	def deepValidate(passport: Map[String, String]): Boolean = {
		false
	}

	def star7(): Unit = {
		val passports = parseFile(lines)
		println(passports.count(fieldsExist))
	}
}
