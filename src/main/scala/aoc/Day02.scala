package aoc

import aoc.int.{IntComputer, IntComputerProgress}

import scala.annotation.tailrec
import scala.io.Source

object Day02 extends App {

  val source = Source.fromFile("input/day02.input")
  val sourceCode = source.getLines().mkString.split(',').map(_.toLong).toList

  def intComputer(x: Long, y: Long, code: List[Long] = sourceCode): Int = {
    val result = IntComputer.runComputer(IntComputerProgress(code.updated(1, x).updated(2, y)))
    result.code.find(_._1 == 0).get._2.toInt
  }

  println(s"Code: ${intComputer(12, 2)}")

  val combinations = for {
    noun <- 0 to 99
    verb <- 0 to 99
  } yield (noun, verb)

  for ((noun, verb) <- combinations.find { case (noun, verb) => intComputer(noun, verb) == 19690720}) {
    println(s"Combination: $noun/$verb - Code: ${(100 * noun) + verb}")
  }
}
