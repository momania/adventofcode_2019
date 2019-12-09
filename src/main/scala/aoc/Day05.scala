package aoc

import aoc.int.{IntComputer, IntComputerProgress}

import scala.io.Source

object Day05 extends App {

  val source = Source.fromFile("input/day05.input")
  val sourceCode = source.getLines().mkString.split(',').map(_.toLong).toList

  val sampleInput = List(3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99L)
  val sampleOutput = IntComputer.runOpCode(IntComputerProgress(sampleInput, List(9)))
  println(s"Sample output: ${sampleOutput.output}")

  val output = IntComputer.runOpCode(IntComputerProgress(sourceCode, List(1)))
  println(s"Output: ${output.output}")

  val outputTwo = IntComputer.runOpCode(IntComputerProgress(sourceCode, List(5)))
  println(s"Output two: ${outputTwo.output}")
}
