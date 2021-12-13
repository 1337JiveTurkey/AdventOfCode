package tyool2021

import scala.collection.mutable

object Day10 extends Main {
	def main(args: Array[String]): Unit = {
		star22()
	}

	def star21(): Unit = {
		val lines = fileLines("Star21.txt")
		println((lines map scoreLine).sum)
	}

	def star22(): Unit = {
		val lines = fileLines("Star21.txt")
		val scores = lines map scoreIncompleteLine filter (_ != 0)
		val sorted = scores.sorted
		println(scores.length)
		println(sorted(sorted.length / 2))
	}

	def scoreLine(line: String): Int = {
		val stack = mutable.Stack[Char]()
		for (c <- line) {
			if (c == '(')
				stack.push(')')
			else if (c == '[')
				stack.push(']')
			else if (c == '{')
				stack.push('}')
			else if (c == '<')
				stack.push('>')
			else if (c == ')') {
				if (stack.isEmpty || stack.pop() != c) {
					return 3
				}
			}
			else if (c == ']') {
				if (stack.isEmpty || stack.pop() != c) {
					return 57
				}
			}
			else if (c == '}') {
				if (stack.isEmpty || stack.pop() != c) {
					return 1197
				}
			}
			else if (c == '>') {
				if (stack.isEmpty || stack.pop() != c) {
					return 25137
				}
			}
		}
		0
	}

	def completeLine(line: String): Option[IndexedSeq[Char]] = {
		val stack = mutable.Stack[Char]()
		for (c <- line) {
			if (c == '(')
				stack.push(')')
			else if (c == '[')
				stack.push(']')
			else if (c == '{')
				stack.push('}')
			else if (c == '<')
				stack.push('>')
			else if (c == ')') {
				if (stack.isEmpty || stack.pop() != c) {
					return None
				}
			}
			else if (c == ']') {
				if (stack.isEmpty || stack.pop() != c) {
					return None
				}
			}
			else if (c == '}') {
				if (stack.isEmpty || stack.pop() != c) {
					return None
				}
			}
			else if (c == '>') {
				if (stack.isEmpty || stack.pop() != c) {
					return None
				}
			}
		}
		if (stack.isEmpty) {
			None
		} else {
			Some(IndexedSeq.from(stack))
		}
	}

	def scoreIncompleteLine(line: String): Long = {
		val toComplete = completeLine(line)
		if (toComplete.isDefined) {
			var score = 0L
			for (c <- toComplete.get) {
				if (c == ')') {
					score = 5 * score + 1
				}
				else if (c == ']') {
					score = 5 * score + 2
				}
				else if (c == '}') {
					score = 5 * score + 3
				}
				else if (c == '>') {
					score = 5 * score + 4
				}
			}
			score
		}
		else {
			0
		}
	}
}
