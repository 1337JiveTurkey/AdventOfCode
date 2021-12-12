package tyool2021

import scala.collection.mutable

object Day10 extends Main {
	def main(args: Array[String]): Unit = {
		star21()
	}

	def star21(): Unit = {
		val lines = fileLines("Star21.txt")
		println((lines map scoreLine).sum)
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
}
