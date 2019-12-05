package aoc

import scala.annotation.tailrec
import scala.io.Source

object Day05 extends App {

  val source = Source.fromFile("input/day05.input")
  val sourceCode = source.getLines().mkString.split(',').map(_.toInt).toList

  case class Instruction(operation: Int, modes: List[Int])
  object Instruction {
    def apply(code: Int): Instruction = {
      if (code < 99) {
        Instruction(code, Nil)
      }
      else {
        val strCode = code.toString
        // char to String first, then to Int, otherwise we get the character code instead of value
        val modes = strCode.dropRight(2).toList.map(_.toString.toInt).reverse
        Instruction(strCode.takeRight(2).toInt, modes)
      }
    }
  }
  @tailrec def runOpCode(input: Int, index: Int, code: List[Int], output: List[Int]): List[Int] = {

    @inline def getPositionValueAt(i: Int) = code(code(i))
    @inline def getImmedateValueAt(i: Int) = code(i)
    @inline def getValue(i: Int, mode: Option[Int]) = mode.getOrElse(0) match {
      case 0 => getPositionValueAt(i)
      case 1 => getImmedateValueAt(i)
    }

    val instruction = Instruction(code(index))
    println(s"Index: $index - instruction: $instruction")

    instruction match {
      case Instruction(99, _) =>
        output
      case Instruction(3, _) =>
        val position = getImmedateValueAt(index + 1)
        runOpCode(input, index + 2, code.updated(position, input), output)
      case Instruction(4, modes) =>
        val out = getValue(index + 1, modes.headOption)
        runOpCode(input, index + 2, code, output :+ out)
      case Instruction(1, modes) =>
        val x = getValue(index + 1, modes.headOption)
        val y = getValue(index + 2, modes.drop(1).headOption)
        val updatedCode = code.updated(getImmedateValueAt(index + 3), x + y)
        runOpCode(input, index + 4, updatedCode, output)
      case Instruction(2, modes) =>
        val x = getValue(index + 1, modes.headOption)
        val y = getValue(index + 2, modes.drop(1).headOption)
        val updatedCode = code.updated(getImmedateValueAt(index + 3), x * y)
        runOpCode(input, index + 4, updatedCode, output)
    }
  }

  def intComputer(input: Int, code: List[Int] = sourceCode) = {
    runOpCode(input, 0, code, Nil)
  }

  val output = intComputer(1)
  println(s"Output: $output")

  val outputTwo = intComputer(5)
  println(s"Output two: $outputTwo")
}
