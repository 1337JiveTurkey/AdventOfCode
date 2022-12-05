package tyool2018

import scala.collection.immutable.{TreeMap, TreeSet}
import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}
import scala.util.matching.Regex

object Day7 extends Main {
	def main(args: Array[String]): Unit = {
		star1()
	}

	val LinePattern: Regex = """Step (\w) must be finished before step (\w) can begin.""".r

	def star1(): Unit = {
		val lines = fileLines("Day7.txt")
		var tasks = initTasks
		for (LinePattern(first, second) <- lines) {
			tasks(second).addTask(tasks(first))
		}
		val queue = new mutable.StringBuilder()
		while (tasks.nonEmpty) {
			var taskToRemove: String = null
			breakable {
				for ((taskName, task) <- tasks) {
					if (task.dependsOn.isEmpty) {
						queue.append(taskName)
						for (otherTask <- tasks.values) {
							otherTask.removeTask(task)
						}
						taskToRemove = taskName
						break()
					}
				}
			}
			if (taskToRemove != null) {
				tasks = tasks.removed(taskToRemove)
			}
		}
		println(queue.result())
	}

	val initTasks: Map[String, Task] = {
		val mb = TreeMap.newBuilder[String, Task]
		for (char <- 'A' to 'Z') {
			mb.addOne(char.toString, Task(char.toString))
		}
		mb.result()
	}

	case class Task(letter: String) extends Ordered[Task] {
		val time: Int = letter.head.toInt - 'A'.toInt + 60
		var dependsOn: Set[Task] = Set.empty[Task]
		def addTask(task: Task): Unit = {
			dependsOn = dependsOn + task
		}
		def removeTask(task: Task): Unit = {
			dependsOn = dependsOn - task
			if (dependsOn.isEmpty) {
				println(this + " is ready to enqueue")
			}
		}

		override def compare(that: Task): Int = letter.compareTo(that.letter)
	}
}
