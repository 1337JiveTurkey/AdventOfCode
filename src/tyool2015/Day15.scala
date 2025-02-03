package tyool2015

object Day15 extends Main {
	def main(args: Array[String]): Unit = {
		star1()
	}

	def star1(): Unit = {
		val lines = fileLines("Day15.txt")
		val ingredients = parseIngredients(lines).toList
	}

	def findOptimum(ingredients: List[Ingredient], spaceLeft: Int, totals: Ingredient): Int = {
		if (ingredients.tail.isEmpty) {
			ingredients
		}
		0
	}

	private val ingredientLine = """\w+: capacity (-?\d), durability (-?\d), flavor (-?\d), texture (-?\d), calories (-?\d)""".r

	def parseIngredients(lines: IndexedSeq[String]): IndexedSeq[Ingredient] = {
		for (ingredientLine(capacity, durability, flavor, texture, calories) <- lines) yield {
			Ingredient(capacity.toInt, durability.toInt, flavor.toInt, texture.toInt, calories.toInt)
		}
	}

	case class Ingredient(capacity: Int, durability: Int, flavor: Int, texture: Int, calories: Int) {

	}
}
