package aoc

import scala.annotation.tailrec
import scala.io.Source

object Day03 extends App {

  val source = Source.fromFile("input/day03.input")
  val wireOne :: wireTwo :: Nil =
    source.getLines().toList.map(_.split(',').toList)

  case class Coordinate(x: Int, y: Int) {
    def up(by: Int) = Coordinate(x + by, y)
    def down(by: Int) = Coordinate(x - by, y)
    def right(by: Int) = Coordinate(x, y + by)
    def left(by: Int) = Coordinate(x, y - by)
    def distanceTo(other: Coordinate) = math.abs(x - other.x) + math.abs(y - other.y)
  }
  object Coordinate {
    lazy val zero = Coordinate(0, 0)
  }

  //  val wireOne = "R75,D30,R83,U83,L12,D49,R71,U7,L72".split(',').toList
//  val wireTwo = "U62,R66,U55,R34,D71,R55,D58,R83".split(',').toList
//  val wireOne = "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51".split(',').toList
//  val wireTwo = "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7".split(',').toList
  println(s"1: $wireOne")
  println(s"2: $wireTwo")

  def collectCoordinates(currentPos: Coordinate,
                         direction: String,
                         steps: Int): List[Coordinate] = {
    val movement = direction match {
      case "U" => i: Int => currentPos.up(i)
      case "D" => i: Int => currentPos.down(i)
      case "R" => i: Int => currentPos.right(i)
      case "L" => i: Int => currentPos.left(i)
    }
    (1 to steps).map(movement).toList
  }

  @tailrec def followWire(wire: List[String],
                          currentPosition: Coordinate = Coordinate.zero,
                          path: List[Coordinate] = Nil): List[Coordinate] = {
    wire match {
      case Nil => path
      case instruction :: rest =>
        val (direcion , steps) = instruction.splitAt(1)
        val positions = collectCoordinates(currentPosition, direcion, steps.toInt)
        followWire(rest, positions.last, path ::: positions)
    }
  }

  val pathOne = followWire(wireOne)
  val pathTwo = followWire(wireTwo)

  println(s"Path1: ${pathOne.size}")
  println(s"Path2: ${pathTwo.size}")

  val intersections = pathOne.intersect(pathTwo)
  println(s"Intersections: $intersections")

  val distances = intersections.map{_.distanceTo(Coordinate.zero)}
  println(s"Closest: ${distances.min}")

  val steps = intersections.map { intersection =>
    val stepsOne = pathOne.takeWhile(_ != intersection) :+ intersection
    val stepsTwo = pathTwo.takeWhile(_ != intersection) :+ intersection
    stepsOne.size + stepsTwo.size
  }
  println(s"Minimal steps: ${steps.min}")
}
