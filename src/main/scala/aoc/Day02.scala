package aoc

import scala.annotation.tailrec
import scala.io.Source

object Day02 extends App {

  val source = Source.fromFile("input/day02.input")
  val sourceCode = source.getLines().mkString.split(',').map(_.toInt).toList

  @tailrec def runOpCode(index: Int, code: List[Int]): List[Int] = {
   code.slice(index, index + 4) match {
     case 99 :: _ => code
     case instr :: x :: y :: z :: Nil =>
       val update = instr match {
         case 1 => code(x) + code(y)
         case 2 => code(x) * code(y)
       }
       val updatedCode = code.updated(z, update)
       runOpCode(index + 4, updatedCode)
   }
  }

  def intComputer(x: Int, y: Int, code: List[Int] = sourceCode): Int = {
    runOpCode(0, code.updated(1, x).updated(2, y)).head
  }

  println(s"Code: ${intComputer(12, 2)}")


  val combinations = for {
    noun <- 0 to 99
    verb <- 0 to 99
  } yield (noun, verb)

  for ((noun, verb) <- combinations.find { case (noun, verb) => intComputer(noun, verb) == 19690720}) {
    println(s"Combination: $noun/$verb - Code: ${(100 * noun) + verb}")
  }

}
