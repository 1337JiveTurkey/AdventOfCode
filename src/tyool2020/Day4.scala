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
		validateByr(passport) &&
		validateIyr(passport) &&
		validateEyr(passport) &&
		validateHgt(passport) &&
		validateHcl(passport) &&
		validateEcl(passport) &&
		validatePid(passport)
	}

	val fourDigit: Regex = """(\d{4})""".r

	def validateByr(passport: Map[String, String]): Boolean = {
		passport.get("byr") match {
			case Some(fourDigit(byr)) => (1920 to 2020) contains byr.toInt
			case _ => false
		}
	}

	def validateIyr(passport: Map[String, String]): Boolean = {
		passport.get("iyr") match {
			case Some(fourDigit(iyr)) => (2010 to 2020) contains iyr.toInt
			case _ => false
		}
	}

	def validateEyr(passport: Map[String, String]): Boolean = {
		passport.get("eyr") match {
			case Some(fourDigit(eyr)) => (2020 to 2030) contains eyr.toInt
			case _ => false
		}
	}

	val hgtCm: Regex = """(\d+)cm""".r
	val hgtIn: Regex = """(\d+)in""".r

	def validateHgt(passport: Map[String, String]): Boolean = {
		passport.get("hgt") match {
			case Some(hgtCm(cms)) => (150 to 193) contains cms.toInt
			case Some(hgtIn(ins)) => (59 to 76) contains ins.toInt
			case _ => false
		}
	}

	val hclColor: Regex = """#([0-9a-f]{6})""".r

	def validateHcl(passport: Map[String, String]): Boolean = {
		passport.get("hcl") match {
			case Some(hclColor(_)) => true
			case _ => false
		}
	}

	val eclColor: Regex = """(amb|blu|brn|gry|grn|hzl|oth)""".r

	def validateEcl(passport: Map[String, String]): Boolean = {
		passport.get("ecl") match {
			case Some(eclColor(_)) => true
			case _ => false
		}
	}

	val nineDigit: Regex = """(\d{9})""".r

	def validatePid(passport: Map[String, String]): Boolean = {
		passport.get("pid") match {
			case Some(nineDigit(_)) => true
			case _ => false
		}
	}


	def star7(): Unit = {
		val passports = parseFile(lines)
		println(passports.count(deepValidate))
	}
}
