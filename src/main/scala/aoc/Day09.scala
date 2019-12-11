package aoc

import aoc.int.{IntComputer, IntComputerProgress, IntComputerState}

import scala.annotation.tailrec
import scala.io.Source

object Day09 extends App {

  val source = Source.fromFile("input/day09.input")
  val sourceCode = source.getLines().mkString.split(',').map(_.toLong).toList

//  val testCode = List(109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99L)
//  val testCode2 = List(1102,34915192,34915192,7,4,7,99,0L)
//  val testCode3 = List(104,1125899906842624L,99)
//
//  val testResult = IntComputer.runOpCode(IntComputerProgress(testCode))
//  println(s"Test result: ${testResult.output.reverse} - equals input: ${testResult.output.reverse == testCode}")
//
//  val testResult2 = IntComputer.runOpCode(IntComputerProgress(testCode2))
//  println(s"Test result2: ${testResult2.output} - length: ${testResult2.output.head.toString.length}")
//
//  val testResult3 = IntComputer.runOpCode(IntComputerProgress(testCode3))
//  println(s"Test result3: ${testResult3.output} - equals large number: ${testResult3.output.head == testCode3.tail.head}")

  val testMode = IntComputer.runComputer(IntComputerProgress(sourceCode, List(1)))
  println(s"Test mode key: ${testMode}")
  println(s"Test mode key: ${testMode.output}")

  val boostMode = IntComputer.runComputer(IntComputerProgress(sourceCode, List(2)))
  println(s"Boost mode key: ${boostMode}")
  println(s"Boost mode key: ${boostMode.output}")


}
