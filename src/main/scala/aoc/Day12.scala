package aoc

import scala.annotation.tailrec
import scala.io.Source

object Day12 extends App {

  val source = Source.fromFile("input/day12.input")
  val sourceCode = source.getLines().toList

  val names = List("Io", "Europa", "Ganymede", "Callisto")

  case class Measure(x: Int, y: Int, z: Int)
  case class Moon(name: String, position: Measure, velocity: Measure)

  val moons = sourceCode.zipWithIndex.map{ case (line, index) =>
    val elems = line.tail.dropRight(1)
    val x :: y :: z :: Nil = elems.split(',').map { e => e.split('=').last.toInt}.toList
    Moon(names(index), Measure(x, y, z), Measure(0, 0, 0))
  }

  @tailrec def simulation(steps: Int, moons: List[Moon]): List[Moon] = {
    if (steps > 0) {
      val updateMoons = moons
      simulation(steps - 1, updateMoons)
    } else {
      moons
    }
  }

  val simulatedMoons = simulation(1000, moons)
  val totalEnergy = totalEnergy(simulatedMoons)
  println(s"Total energy: $totalEnergy")

  def totalEnergy(moons: List[Moon]): Long = {
    moons.map { moon =>
      measureEnergy(moon.position) + measureEnergy(moon.velocity)
    }.sum
  }

  def measureEnergy(measure: Measure): Long = {
    math.abs(measure.x) + math.abs(measure.y) + math.abs(measure.z)
  }
  println(s"Moons: $moons")
}
