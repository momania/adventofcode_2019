package aoc

import aoc.int.{IntComputer, IntComputerProgress, IntComputerState}

import scala.annotation.tailrec
import scala.io.Source

object Day07 extends App {

  val source = Source.fromFile("input/day07.input")
  val sourceCode = source.getLines().mkString.split(',').map(_.toLong).toList

  val testCode = List(3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0L)
  val testCode2 = List(3,23,3,24,1002,24,10,24,1002,23,-1,23, 101,5,23,23,1,24,23,23,4,23,99,0,0L)
  val testCode3 = List(3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33, 1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0L)
  val testCode4 = List(3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26, 27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5L)
  val testCode5 = List(3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54, -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4, 53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10L)

  def runPhases(phases: List[Int], code: List[Long]): Long = {
    phases.foldLeft(0L) { case (input, phase) =>
      println(s"Phase: $phase - Input: $input")
      val startProgress = IntComputerProgress(code, List(phase, input))
      IntComputer.runComputer(startProgress).output.head
    }
  }

  val phases = (0 to 4).toList
  val signals = phases.permutations.map(p => runPhases(p, sourceCode))
  println(s"Signal: ${signals.max}")



  def runFeedbackLoop(phases: List[Int], code: List[Long]): Long = {
    val amps = phases.map { phase => IntComputerProgress(code, List(phase))}

    @tailrec def runInternal(signal: Long, todo: List[IntComputerProgress], done: List[IntComputerProgress]): List[IntComputerProgress] = {
      todo match {
        case head :: rest =>
          val updatedHead = head.copy(input = head.input :+ signal)
          val updatedDone = IntComputer.runComputer(updatedHead)
          runInternal(updatedDone.output.last, rest, done :+ updatedDone)
        case Nil =>
          done

      }
    }

    @tailrec def runUntilHalted(amps: List[IntComputerProgress]): List[IntComputerProgress] = {
      if (amps.exists(_.state == IntComputerState.Halted)) {
        amps
      } else {
        val signal = amps.lastOption.flatMap(_.output.lastOption).getOrElse(0L)
        runUntilHalted(runInternal(signal, amps, Nil))
      }
    }

    val round1 = runUntilHalted(amps)
    println(s"Progress round1: $round1")
    round1.head.output.head
  }

  val phases2 = (5 to 9).toList
  val signals2 = phases2.permutations.map(p => runFeedbackLoop(p, sourceCode))

  println(s"Signal 2: ${signals2.max}")
}
