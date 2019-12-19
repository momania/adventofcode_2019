package aoc

import aoc.int.{IntComputer, IntComputerProgress}

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Random

object Day17 extends App {

  val source = Source.fromFile("input/day17.input")
  val sourceCode = source.getLines().mkString.split(',').map(_.toLong).toList

  val res = IntComputer.runComputer(IntComputerProgress(sourceCode))
  println(s"Res: ${res.state}")
  println(s"Res: ${res.output}")

  case class Coordinate(x: Int, y: Int)

  def ouputMap(map: Map[Coordinate, Int]): Unit = {

    print("\u001b[2J")

    val keys = map.keySet
    val allX = keys.map(_.x)
    val allY = keys.map(_.y)

    for (y <- allY.min to allY.max) {
      for {x <- allX.min to allX.max} {
        print(map(Coordinate(x, y)).toChar)
      }
      println()
    }
  }

  def isIntersection(position: Coordinate, map: Map[Coordinate, Int]): Boolean = {
    val neighbors = List(
      position.copy(x = position.x - 1),
      position.copy(x = position.x + 1),
      position.copy(y = position.y - 1),
      position.copy(y = position.y + 1)
    )
    map(position).toChar == '#' &&
      neighbors.forall(pos => map.get(pos).exists(_.toChar == '#'))
  }

  // create map
  val map = res.output.foldLeft((0, 0, Map.empty[Coordinate, Int])){ case ((x, y, map), o) =>
    if (o == 10) (0, y + 1, map) else (x + 1, y, map + (Coordinate(x, y) -> o.toInt))
  }._3

  ouputMap(map)

  val intersections = map.keySet.filter(pos => isIntersection(pos, map))
  val alignmentParamsSum = intersections.map{pos => pos.x * pos.y}.sum
  println(s"Alignment parameters sum: $alignmentParamsSum")
}
