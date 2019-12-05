package aoc

import scala.annotation.tailrec
import scala.io.Source

object Day05 extends App {

  val source = Source.fromFile("input/day05.input")
  val sourceCode = source.getLines().mkString.split(',').map(_.toInt).toList

  def runOpCode(input: Int, index: Int, code: List[Int]): List[Int] = {
    println(s"Index: $index")
    // mode 1 is immediate
    val instruction = code(index).toString
    println(s"Instruction: $instruction")
    instruction.toList match {
      case '9' :: '9' :: Nil => // halt
        println("HALT")
      case modeY :: modeX :: '0' :: '1' :: Nil =>
        val x = if(modeX == '1') code(index + 1) else code(code(index + 1))
        val y = if(modeY == '1') code(index + 2) else code(code(index + 2))
        println(s"Sum values, using modeX $modeX and modeY $modeY - x: $x - y: $y and store at ${code(index + 3)}")
        val updatedCode = code.updated(code(index + 3), x + y)
        runOpCode(input, index + 4, updatedCode)
      case modeX :: '0' :: '1' :: Nil =>
        val x = if(modeX == '1') code(index + 1) else code(code(index + 1))
        val y = code(code(index + 2))
        println(s"Sum values, using modeX $modeX only - x: $x - y: $y and store at ${code(index + 3)}")
        val updatedCode = code.updated(code(index + 3), x + y)
        runOpCode(input, index + 4, updatedCode)
      case '1' :: Nil =>
        val x = code(code(index + 1))
        val y = code(code(index + 2))
        println(s"Sum values - x: $x - y: $y and store at ${code(index + 3)}")
        val updatedCode = code.updated(code(index + 3), code(code(index + 1)) + code(code(index + 2)))
        runOpCode(input, index + 4, updatedCode)
      case modeY :: modeX :: '0' :: '2' :: Nil =>
        val x = if(modeX == '1') code(index + 1) else code(code(index + 1))
        val y = if(modeY == '1') code(index + 2) else code(code(index + 2))
        println(s"Multiply values, using modeX $modeX and modeY $modeY - x: $x - y: $y and store at ${code(index + 3)}")
        val updatedCode = code.updated(code(index + 3), x * y)
        runOpCode(input, index + 4, updatedCode)
      case modeX :: '0' :: '2' :: Nil =>
        val x = if(modeX == '1') code(index + 1) else code(code(index + 1))
        val y = code(code(index + 2))
        println(s"Multiply values, using modeX $modeX only - x: $x - y: $y and store at ${code(index + 3)}")
        val updatedCode = code.updated(code(index + 3), x * y)
        runOpCode(input, index + 4, updatedCode)
      case '2' :: Nil =>
        val x = code(code(index + 1))
        val y = code(code(index + 2))
        println(s"Multiply values - x: $x - y: $y and store at ${code(index + 3)}")
        val updatedCode = code.updated(code(index + 3), x * y)
        runOpCode(input, index + 4, updatedCode)
      case mode :: '0' :: '3' :: Nil =>
      case '3' :: Nil =>
        println(s"Put input at ${code(index + 1)}")
        runOpCode(input, index + 2, code.updated(code(index + 1), input))
      case mode :: '0' :: '4' :: Nil =>
        val output = if(mode == '1') code(index + 1) else code(code(index + 1))
        println(s"OUTPUT mode $mode: $output")
        runOpCode(input, index + 2, code)
      case '4' :: Nil =>
        println(s"OUTPUT: ${code(code(index + 1))}")
        runOpCode(input, index + 2, code)
    }

    List.empty
  }

  def intComputer(input: Int, code: List[Int] = sourceCode) {
    runOpCode(input, 0, code)
  }

  intComputer(1)




}
