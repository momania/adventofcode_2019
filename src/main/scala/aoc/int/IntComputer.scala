package aoc.int

import scala.annotation.tailrec

sealed trait IntComputerState
object IntComputerState {
  case object Running extends IntComputerState
  case object Halted extends IntComputerState
  case object AwaitingInput extends IntComputerState
}
object IntComputerProgress {

  private def simpleCodeToAddressedCode(code: List[Long]): List[(Int, Long)] = {
    code.zipWithIndex.map { case (c, i) => i -> c}
  }
  def apply(code: List[Long]): IntComputerProgress = {
    IntComputerProgress(IntComputerState.Running, 0, simpleCodeToAddressedCode(code), Nil, Nil, 0)
  }
  def apply(code: List[Long], input: List[Long]): IntComputerProgress = {
    IntComputerProgress(IntComputerState.Running, 0, simpleCodeToAddressedCode(code), input, Nil, 0)
  }
}

case class IntComputerProgress(state: IntComputerState,
                               index: Int,
                               code: List[(Int, Long)],
                               input: List[Long],
                               output: List[Long],
                               relativeBase: Int)

object IntComputer {

  private implicit def boolToLong(bool: Boolean) = if (bool) 1L else 0L

  private def extractInstructionAndModes(code: Long): (Int, List[Int]) = {
    val strCode = code.toString
    val modes = strCode.dropRight(2).toList.map(_.toString.toInt).reverse
    (strCode.takeRight(2).toInt, modes)
  }

  @tailrec def runOpCode(progress: IntComputerProgress): IntComputerProgress = {

    @inline def getAddressValue(i: Int) = {
      if (i < 0) sys.error(s"Invalid index $i")
      progress.code.find(_._1 == i).map(_._2).getOrElse(0L)
    }

    val (instruction, modes) = extractInstructionAndModes(getAddressValue(progress.index))
    println(s"Index: ${progress.index} - instruction: $instruction - modes: $modes")

    @inline def getPositionValueAt(i: Int) = getAddressValue(getAddressValue(i).toInt)
    @inline def getRelativeValueAt(i: Int) = {
      println(s"Getting relative value at $i - address value: ${getAddressValue(i).toInt} - base: ${progress.relativeBase}")
      getAddressValue(progress.relativeBase + getAddressValue(i).toInt)
    }
    @inline def getValue(i: Int, mode: Int) = mode match {
      case 0 => getPositionValueAt(i)
      case 1 => getAddressValue(i)
      case 2 => getRelativeValueAt(i)
    }

    @inline def getParameter(i: Int, defaultMode: Int = 0 ) = getValue(progress.index + 1 + i, modes.drop(i).headOption.getOrElse(defaultMode))

    @inline def xyOperation(op: (Long, Long) => Long): List[(Int, Long)] = {
      val address = getAddressValue(progress.index + 3).toInt
      val x = getParameter(0)
      val y = getParameter(1)
      println(s"X: $x - Y: $y")
      val value = op(x, y)
      updateAddress(address, value, modes.drop(2).headOption.getOrElse(0))
    }

    @inline def nextIndexOperation(op: Int => Boolean) = {
      if (op(getParameter(0).toInt)) getParameter(1).toInt else progress.index + 3
    }

    def updateAddress(address: Int, value: Long, mode: Int): List[(Int, Long)] = {
      val realAddress = mode match {
        case 2 => progress.relativeBase + address
        case _ => address
      }
      println(s"Updating $realAddress with $value")
      progress.code.filterNot(_._1 == realAddress) :+ realAddress -> value
    }

    val updatedProgress = instruction match {
      case 99 => progress.copy(state = IntComputerState.Halted)
      case 1 => progress.copy(state = IntComputerState.Running, index = progress.index + 4, code = xyOperation(_ + _))
      case 2 => progress.copy(state = IntComputerState.Running, index = progress.index + 4, code = xyOperation(_ * _))
      case 3 if progress.input.isEmpty => progress.copy(state = IntComputerState.AwaitingInput)
      case 3 => progress.copy(state = IntComputerState.Running, index = progress.index + 2, code = updateAddress(getAddressValue(progress.index + 1).toInt, progress.input.head, modes.headOption.getOrElse(0)), input = progress.input.tail)
      case 4 =>
        val addedOuput = getParameter(0)
        println(s"Adding output $addedOuput")
        progress.copy(state = IntComputerState.Running, index = progress.index + 2, output = addedOuput :: progress.output)
      case 5 => progress.copy(state = IntComputerState.Running, index = nextIndexOperation(_ != 0))
      case 6 => progress.copy(state = IntComputerState.Running, index = nextIndexOperation(_ == 0))
      case 7 => progress.copy(state = IntComputerState.Running, index = progress.index + 4, code = xyOperation(_ < _))
      case 8 => progress.copy(state = IntComputerState.Running, index = progress.index + 4, code = xyOperation(_ == _))
      case 9 =>
        val adjustment = getParameter(0)
        println(s"Adjusting relative base by $adjustment - current base: ${progress.relativeBase}")
        progress.copy(state = IntComputerState.Running, index = progress.index + 2, relativeBase = progress.relativeBase + adjustment.toInt)
    }

    updatedProgress.state match {
      case IntComputerState.Halted => updatedProgress
      case IntComputerState.AwaitingInput => updatedProgress
      case IntComputerState.Running => runOpCode(updatedProgress)
    }
  }
}
