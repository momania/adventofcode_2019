package aoc.int

import scala.annotation.tailrec

sealed trait IntComputerState
object IntComputerState {
  case object Running extends IntComputerState
  case object Halted extends IntComputerState
  case object AwaitingInput extends IntComputerState
}
object IntComputerProgress {
  def apply(code: List[Int]): IntComputerProgress = {
    IntComputerProgress(IntComputerState.Running, 0, code, Nil, Nil)
  }
  def apply(code: List[Int], input: List[Int]): IntComputerProgress = {
    IntComputerProgress(IntComputerState.Running, 0, code, input, Nil)
  }
}

case class IntComputerProgress(state: IntComputerState,
                               index: Int,
                               code: List[Int],
                               input: List[Int],
                               output: List[Int])

object IntComputer {

  private implicit def boolToInt(bool: Boolean) = if (bool) 1 else 0

  private def extractInstructionAndModes(code: Int): (Int, List[Int]) = {
    val strCode = code.toString
    val modes = strCode.dropRight(2).toList.map(_.toString.toInt).reverse
    (strCode.takeRight(2).toInt, modes)
  }

  @tailrec def runOpCode(progress: IntComputerProgress): IntComputerProgress = {

    val (instruction, modes) = extractInstructionAndModes(progress.code(progress.index))
    println(s"Index: ${progress.index} - instruction: $instruction")

    @inline def getImmediateValueAt(i: Int) = progress.code(i)
    @inline def getPositionValueAt(i: Int) = getImmediateValueAt(progress.code(i))
    @inline def getValue(i: Int, mode: Option[Int]) = {
      if (mode.contains(1)) getImmediateValueAt(i) else getPositionValueAt(i)
    }

    @inline def getParameter(i: Int) = getValue(progress.index + 1 + i, modes.drop(i).headOption)

    @inline def xyOperation(op: (Int, Int) => Int): List[Int] = {
      progress.code.updated(getImmediateValueAt(progress.index + 3), op(getParameter(0), getParameter(1)))
    }

    @inline def nextIndexOperation(op: Int => Boolean) = {
      if (op(getParameter(0))) getParameter(1) else progress.index + 3
    }

    val updatedProgress = instruction match {
      case 99 => progress.copy(state = IntComputerState.Halted)
      case 1 => progress.copy(state = IntComputerState.Running, index = progress.index + 4, code = xyOperation(_ + _))
      case 2 => progress.copy(state = IntComputerState.Running, index = progress.index + 4, code = xyOperation(_ * _))
      case 3 if progress.input.isEmpty => progress.copy(state = IntComputerState.AwaitingInput)
      case 3 => progress.copy(state = IntComputerState.Running, index = progress.index + 2, code = progress.code.updated(getImmediateValueAt(progress.index + 1), progress.input.head), input = progress.input.tail)
      case 4 => progress.copy(state = IntComputerState.Running, index = progress.index + 2, output = getParameter(0) :: progress.output)
      case 5 => progress.copy(state = IntComputerState.Running, index = nextIndexOperation(_ != 0))
      case 6 => progress.copy(state = IntComputerState.Running, index = nextIndexOperation(_ == 0))
      case 7 => progress.copy(state = IntComputerState.Running, index = progress.index + 4, code = xyOperation(_ < _))
      case 8 => progress.copy(state = IntComputerState.Running, index = progress.index + 4, code = xyOperation(_ == _))
    }
    updatedProgress.state match {
      case IntComputerState.Halted => updatedProgress
      case IntComputerState.AwaitingInput => updatedProgress
      case IntComputerState.Running => runOpCode(updatedProgress)
    }
  }
}
