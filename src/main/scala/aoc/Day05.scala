package aoc

import scala.annotation.tailrec
import scala.io.Source

object Day05 extends App {

  val source = Source.fromFile("input/day05.input")
  val sourceCode = source.getLines().mkString.split(',').map(_.toInt).toList

  def extractInstructionAndModes(code: Int): (Int, List[Int]) = {
    if (code < 99) { (code, Nil) }
    else {
      val strCode = code.toString
      // char to String first, then to Int, otherwise we get the character code instead of value
      val modes = strCode.dropRight(2).toList.map(_.toString.toInt).reverse
      (strCode.takeRight(2).toInt, modes)
    }
  }

  implicit def boolToInt(bool: Boolean) = if (bool) 1 else 0

  @tailrec def runOpCode(input: Int, index: Int, code: List[Int], output: List[Int]): List[Int] = {

    val (instruction, modes) = extractInstructionAndModes(code(index))
    println(s"Index: $index - instruction: $instruction")

    @inline def getPositionValueAt(i: Int) = code(code(i))
    @inline def getImmedateValueAt(i: Int) = code(i)
    @inline def getValue(i: Int, mode: Option[Int]) = mode.getOrElse(0) match {
      case 0 => getPositionValueAt(i)
      case 1 => getImmedateValueAt(i)
    }

    def xyOperation(op: (Int, Int) => Int): List[Int] = {
      val x = getValue(index + 1, modes.headOption)
      val y = getValue(index + 2, modes.drop(1).headOption)
      code.updated(getImmedateValueAt(index + 3), op(x, y))
    }

    def nextIndexOperation(op: Int => Boolean) = {
      val param = getValue(index + 1, modes.headOption)
      if (op(param)) getValue(index + 2, modes.drop(1).headOption) else index + 3
    }

    instruction match {
      case 99 =>
        output
      case 1 =>
        val updatedCode = xyOperation(_ + _)
        runOpCode(input, index + 4, updatedCode, output)
      case 2 =>
        val updatedCode = xyOperation(_ * _)
        runOpCode(input, index + 4, updatedCode, output)
      case 3 =>
        val position = getImmedateValueAt(index + 1)
        runOpCode(input, index + 2, code.updated(position, input), output)
      case 4 =>
        val out = getValue(index + 1, modes.headOption)
        runOpCode(input, index + 2, code, output :+ out)
      case 5 =>
        val nextIndex = nextIndexOperation(_ != 0)
        runOpCode(input, nextIndex, code, output)
      case 6 =>
        val nextIndex = nextIndexOperation(_ == 0)
        runOpCode(input, nextIndex, code, output)
      case 7 =>
        val updatedCode = xyOperation(_ < _)
        runOpCode(input, index + 4, updatedCode, output)
      case 8 =>
        val updatedCode = xyOperation(_ == _)
        runOpCode(input, index + 4, updatedCode, output)
    }
  }

  def intComputer(input: Int, code: List[Int] = sourceCode) = {
    runOpCode(input, 0, code, Nil)
  }

  val sampleInput = List(3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99)
  val sampleOutput = intComputer(9, sampleInput)
  println(s"Sample output: $sampleOutput")

  val output = intComputer(1)
  println(s"Output: $output")

  val outputTwo = intComputer(5)
  println(s"Output two: $outputTwo")
}
