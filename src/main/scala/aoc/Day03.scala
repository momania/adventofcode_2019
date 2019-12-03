package aoc

import scala.annotation.tailrec
import scala.io.Source

object Day03 extends App {

  val source = Source.fromFile("input/day03.input")
  val wireOne :: wireTwo :: Nil =
    source.getLines().toList.map(_.split(',').toList)

//  val wireOne = "R75,D30,R83,U83,L12,D49,R71,U7,L72".split(',').toList
//  val wireTwo = "U62,R66,U55,R34,D71,R55,D58,R83".split(',').toList
//  val wireOne = "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51".split(',').toList
//  val wireTwo = "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7".split(',').toList
  println(s"1: $wireOne")
  println(s"2: $wireTwo")

  def collectPositions(currentPos: (Int, Int),
                       direction: String,
                       steps: Int): List[(Int, Int)] = {
    val positions = direction match {
      case "U" => (1 to steps).map(s => currentPos.copy(_1 = currentPos._1 + s))
      case "D" => (1 to steps).map(s => currentPos.copy(_1 = currentPos._1 - s))
      case "R" => (1 to steps).map(s => currentPos.copy(_2 = currentPos._2 + s))
      case "L" => (1 to steps).map(s => currentPos.copy(_2 = currentPos._2 - s))
    }
    positions.toList
  }

  @tailrec def followWire(wire: List[String],
                          currentPos: (Int, Int) = (0, 0),
                          path: List[(Int, Int)] = Nil): List[(Int, Int)] = {
    wire match {
      case Nil => path
      case instruction :: rest =>
        val (direcion , steps) = instruction.splitAt(1)
        val positions = collectPositions(currentPos, direcion, steps.toInt)
        followWire(rest, positions.last, path ::: positions)
    }
  }

  val pathOne = followWire(wireOne)
  val pathTwo = followWire(wireTwo)

  println(s"Path1: ${pathOne.size}")
  println(s"Path2: ${pathTwo.size}")

  val intersections = pathOne.intersect(pathTwo)
  println(s"Intersections: $intersections")

  val distances = intersections.map{ pos => math.abs(pos._1) + math.abs(pos._2)}
  println(s"Closest: ${distances.min}")
}
