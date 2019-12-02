package aoc

import scala.annotation.tailrec
import scala.io.Source

object Day02 extends App {

  val source = Source.fromFile("input/day02.input")
  val sourceCode = source.getLines().mkString.split(',').map(_.toInt).toList

  @tailrec def runOpCode(index: Int, code: List[Int]): List[Int] = {
   code.slice(index, index + 4) match {
     case 1 :: x :: y :: z :: Nil =>
       val updatedCode = code.updated(z, code(x) + code(y))
       runOpCode(index + 4, updatedCode)
     case 2 :: x :: y :: z :: Nil =>
       val updatedCode = code.updated(z, code(x) * code(y))
       runOpCode(index + 4, updatedCode)
     case 99 :: _ => code
     case other =>
       sys.error(s"Unknown input: $other")
   }
  }

  def intComputer(code: List[Int], x: Int, y: Int): Int = {
    runOpCode(0, code.updated(1, x).updated(2, y)).head
  }

  println(s"Code: ${intComputer(sourceCode, 12, 2)}")


  val combinations = (0 to 99).flatMap { n => (0 to 99).map { v => (n,v ) } }
  for ((noun, verb) <- combinations.find { case (noun, verb) => intComputer(sourceCode, noun, verb) == 19690720}) {
    println(s"Combination: $noun/$verb - Code: ${(100 * noun) + verb}")
  }

}
