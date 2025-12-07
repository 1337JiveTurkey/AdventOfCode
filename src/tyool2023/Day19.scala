package tyool2023

import scala.util.matching.Regex
import scala.util.matching.Regex.Groups

object Day19 extends Main {
	def main(args: Array[String]): Unit = {
		star1()
	}

	def star1(): Unit = {
		val lines = fileLines("Day19Prime.txt")
		val stanzas = splitOnBlanks(lines)
		val workflowLines = stanzas(0)
		val partLines = stanzas(1)

		val workflows = parseWorkflows(workflowLines)
		val parts = parseParts(partLines)

		for (workflow <- workflows) {
			println(workflow.ruleText)
		}
	}

	val workflowLine: Regex = """([a-z]+)\{(.+)}""".r
	val rulePattern: Regex = """([xmas])([<>])(\d+):([a-zAR]+)""".r
	val partLine: Regex = """\{x=(\d+),m=(\d+),a=(\d+),s=(\d+)}""".r

	def parseParts(lines: IndexedSeq[String]): IndexedSeq[Part] = {
		for (partLine(x, m, a, s) <- lines)
			yield Part(x.toInt, m.toInt, a.toInt, s.toInt)
	}

	def parseWorkflows(lines: IndexedSeq[String]): IndexedSeq[Workflow] = {
		for (workflowLine(name, rules) <- lines) yield {
			val wf = Workflow(name, rules)
			wf
		}
	}

	case class Part(x: Int, m: Int, a: Int, s: Int) {
		def apply(char: Char): Int = char match {
			case 'x' => x
			case 'm' => m
			case 'a' => a
			case 's' => s
		}
	}

	case class Workflow(name: String, ruleText: String)

	case class Rule(attr: Char, ltgt: Char, value: Int, destination: String) {
		def test(p: Part): Boolean = {
			val v = p.apply(attr)
			if (ltgt == '>') {
				v > value
			} else {
				v < value
			}
		}
	}
}
