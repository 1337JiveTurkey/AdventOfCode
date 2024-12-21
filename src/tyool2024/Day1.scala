package tyool2024

object Day1 extends Main {
	def main(args: Array[String]): Unit = {
		star2()
	}

	def star1(): Unit = {
		val lines = fileLines("Day1.txt")
		val (nums1, nums2) = lines.map(dividedNumbers).map(seq => (seq(0), seq(1))).unzip
		val diffs = for ((a, b) <- nums1.sorted zip nums2.sorted)
			yield Math.abs(b - a)
		println(diffs.sum)
	}

	def star2(): Unit = {
		val lines = fileLines("Day1.txt")
		val (nums1, nums2) = lines.map(dividedNumbers).map(seq => (seq(0), seq(1))).unzip
		var sum = 0
		for (num <- nums1) {
			sum += num * nums2.count(_ == num)
		}
		println(sum)
	}
}
