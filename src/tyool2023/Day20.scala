package tyool2023

import scala.collection.mutable
import scala.util.matching.Regex

object Day20 extends Main {
	def main(args: Array[String]): Unit = {
		star2()
	}

	def star1(): Unit = {
		Network.readFile("Day20.txt")
		Network.buttonPresses = 1000
		while (Network.hasNext) {
			println(Network.next())
		}
		println(Network.numLowPulses * Network.numHighPulses)
	}

	def star2(): Unit = {
		Network.readFile("Day20.txt")
		// jg, rh, jm, hf
		Network.modules("jg").logHighPulses = true
		Network.modules("rh").logHighPulses = true
		Network.modules("jm").logHighPulses = true
		Network.modules("hf").logHighPulses = true
		Network.buttonPresses = 5000
		while (Network.hasNext) {
			Network.next()
		}
	}

	object Network extends Iterator[String] {
		val modules: mutable.Map[String, Module] = mutable.HashMap.empty
		val queue: mutable.Queue[Message] = mutable.Queue.empty

		modules.addOne(Button.name -> Button)
		modules.addOne(Broadcaster.name -> Broadcaster)

		var buttonPresses: Int = 0
		var buttonPressesProcessed: Int = 0

		var numHighPulses: Int = 0
		var numLowPulses: Int = 0

		def hasNext: Boolean = buttonPresses > 0 || queue.lengthIs > 0

		def next(): String = {
			if (queue.isEmpty) {
				if (buttonPresses == 0) {
					throw new IllegalStateException("No more button presses or messages!")
				}
				Button.press()
				buttonPresses -= 1
				buttonPressesProcessed += 1
			}
			val m = queue.dequeue()
			val receiver = modules(m.receiver)
			receiver.receive(m)
			if (m.pulseHigh) {
				numHighPulses += 1
				m.sender + " -high-> " + m.receiver
			} else {
				numLowPulses += 1
				m.sender + " -low-> " + m.receiver
			}
		}

		val linePattern: Regex = """([&%]?)(\w+) -> (.+)""".r

		def readFile(fileName: String): Unit = {
			val lines = fileLines(fileName)
			for (line <- lines) {
				line match {
					case linePattern(prefix, name, connectedTo) => {
						if (prefix.isEmpty) {
							assert(name == Broadcaster.name)
							Broadcaster.wiredTo = connectedTo.split(", ").toList
						}
						if (prefix == "%") {
							val module = new FlipFlop(name)
							modules(name) = module
							module.wiredTo = connectedTo.split(", ").toList
						}
						if (prefix == "&") {
							val module = new Conjunction(name)
							modules(name) = module
							module.wiredTo = connectedTo.split(", ").toList
						}
					}
				}
			}

			for (module <- modules.values) {
				for (name <- module.wiredTo) {
					val target = if (!modules.contains(name)) {
						val foo = new Misc(name)
						modules(name) = foo
						println(s"Created $name")
						foo
					} else {
						modules(name)
					}
					target match {
						case c: Conjunction => c.memory(module.name) = false
						case _ =>
					}
				}
			}
		}
	}

	case class Message(pulseHigh: Boolean, sender: String, receiver: String)

	abstract class Module(val name: String) {
		var wiredTo = List.empty[String]
		var logHighPulses = false

		def receive(m: Message): Unit
		def send(pulseHigh: Boolean): Unit = {
			if (logHighPulses && pulseHigh) {
				println(s"$name sent a high pulse at ${Network.buttonPressesProcessed}")
			}
			for (receiver <- wiredTo) {
				Network.queue.enqueue(Message(pulseHigh, name, receiver))
			}
		}
	}

	class Misc(name: String) extends Module(name) {
		var numHighPulses: Int = 0
		var numLowPulses: Int = 0

		def receive(m: Message): Unit = {
			if (m.pulseHigh) {
				numHighPulses += 1
			} else {
				numLowPulses += 1
			}
		}
	}

	object Button extends Module("button") {
		wiredTo = "broadcaster" :: wiredTo

		def receive(m: Message): Unit = throw new UnsupportedOperationException("Buttons can't receive!")
		def press(): Unit = {
			send(false)
		}
	}

	object Broadcaster extends Module("broadcaster") {
		def receive(m: Message): Unit = send(m.pulseHigh)
	}

	class FlipFlop(name: String) extends Module(name) {
		var isOn = false

		def receive(m: Message): Unit = {
			if (!m.pulseHigh) {
				// Flip the state from off to on or vice versa
				isOn = !isOn
				// When it turns on it sends a high pulse, when it turns off, it sends a low pulse
				send(isOn)
			}
		}
	}

	class Conjunction(name: String) extends Module(name) {
		val memory = mutable.HashMap.empty[String, Boolean]

		def receive(m: Message): Unit = {
			memory(m.sender) = m.pulseHigh
			send(!memory.values.reduce(_ & _))
		}
	}
}
