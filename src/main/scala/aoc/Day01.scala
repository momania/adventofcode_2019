package aoc

import scala.annotation.tailrec
import scala.io.Source

object Day01 extends App {

  val source = Source.fromFile("input/day01.input")
  val lines = source.getLines().map(_.toInt).toList

  val totalFuel = lines.foldLeft(0)(_ + computeFuel(_))
  println(s"Total fuel: $totalFuel")

  val totalFuelPlus = lines.foldLeft(0)(_ + computeFuelPlus(_))
  println(s"Total fuel plus: $totalFuelPlus")

  @tailrec def computeFuelPlus(mass: Int, computed: Int = 0): Int = {
    val fuel = math.max(computeFuel(mass), 0)
    if (fuel > 0) {
      computeFuelPlus(fuel, computed + fuel)
    } else {
      computed
    }
  }

  @inline def computeFuel(mass: Int): Int = {
    (mass / 3) - 2
  }
}
