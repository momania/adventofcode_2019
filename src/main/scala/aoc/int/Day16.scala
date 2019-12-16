package aoc.int

import scala.annotation.tailrec
import scala.io.Source

object Day16 extends App {

  val source = Source.fromFile("input/day16.input")
  val sourceCode = source.getLines().mkString.map(_.toString.toInt).toList

  val pattern = List(0, 1, 0, -1)

  @tailrec def runPhase(inputSignal: List[Int], phase: Int, numPhases: Int, untilPos: Int = 0): List[Int] = {
    println(s"Phase: $phase")
    if (phase == numPhases) {
      inputSignal
    } else {
      val inputLength = inputSignal.length
      val continuationSignal = (1  to inputLength).toList.map { pos =>
        if (pos % 1000 == 0) println(s"Pos: $pos")
        val sum = if (pos > inputLength / 2) {
          inputSignal.drop(pos - 1).sum
        } else if (pos > (inputLength / 3)) {
          inputSignal.slice(pos - 1, pos + pos - 1).sum
        } else {
          val patternStream = pattern.foldLeft(LazyList.empty[Int]) { case (l, x) => l.appendedAll(LazyList.fill(pos)(x)) }
          val stream = LazyList.continually(patternStream).flatten.slice(1, inputLength + 1)
          val zipped = inputSignal.zip(stream).drop(pos - 1)
          zipped.foldLeft(0){case (sum, (x, y)) => x * y + sum}
        }
        math.abs(sum) % 10
      }
      runPhase(continuationSignal, phase + 1, numPhases, untilPos)
    }
  }

  println(s"IN: $sourceCode")
  val startOne = System.currentTimeMillis()
  val outputSignal = runPhase(sourceCode, 0, numPhases = 100)
  println(s"OUT (${System.currentTimeMillis() - startOne} ms): $outputSignal")
  println(s"First 8: ${outputSignal.take(8).mkString}")

  // part 2
  val input2 = List.fill(10000)(sourceCode).flatten
  println(s"Input 2: ${input2.length}")

  val firstSeven = input2.take(7).mkString.toInt
  println(s"Offset: $firstSeven")

//  val output2 = runPhase(input2, 0, 100, firstSeven)
//  println(s"First 7: $firstSeven")
//  println(s"Result: ${output2.slice(firstSeven, firstSeven + 8).mkString}")
}
