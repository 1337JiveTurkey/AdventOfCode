package tyool2015

import org.json._

import scala.jdk.CollectionConverters._

object Day12 extends Main {
	def main(args: Array[String]): Unit = {
		star2()
	}

	def star1(): Unit = {
		val line = fileLine("Day12.json")
		val json = new JSONObject(line)
		println(getNumbers(json))
	}

	def star2(): Unit = {
		val line = fileLine("Day12.json")
		val json = new JSONObject(line)
		println(getNonRedNumbers(json))
	}

	def getNumbers(json: JSONObject): Int = {
		var sum = 0
		for (key <- json.keys().asScala) {
			json.get(key) match {
				case child: JSONObject => sum += getNumbers(child)
				case array: JSONArray =>  sum += getNumbers(array)
				case x: Int => sum += x
				case _: String =>
			}
		}
		sum
	}

	def getNumbers(array: JSONArray): Int = {
		var sum = 0
		for (value <- array.iterator().asScala) {
			value match {
				case child: JSONObject => sum += getNumbers(child)
				case array: JSONArray  => sum += getNumbers(array)
				case x: Int => sum += x
				case _ =>
			}
		}
		sum
	}

	def getNonRedNumbers(json: JSONObject): Int = {
		var sum = 0
		for (key <- json.keys().asScala) {
			json.get(key) match {
				case child: JSONObject => sum += getNonRedNumbers(child)
				case array: JSONArray  => sum += getNonRedNumbers(array)
				case x: Int => sum += x
				case "red" => return 0
				case _: String =>
			}
		}
		sum
	}

	def getNonRedNumbers(array: JSONArray): Int = {
		var sum = 0
		for (value <- array.iterator().asScala) {
			value match {
				case child: JSONObject => sum += getNonRedNumbers(child)
				case array: JSONArray  => sum += getNonRedNumbers(array)
				case x: Int => sum += x
				case _: String =>
			}
		}
		sum
	}
}
