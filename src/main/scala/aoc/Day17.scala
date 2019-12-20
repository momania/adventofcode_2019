package aoc

import aoc.int.{IntComputer, IntComputerProgress}
import aoc.model.{Coordinate, Direction, Turn}

import scala.annotation.tailrec
import scala.io.Source

object Day17 extends App {

  val source = Source.fromFile("input/day17.input")
  val sourceCode = source.getLines().mkString.split(',').map(_.toLong).toList

  val res = IntComputer.runComputer(IntComputerProgress(sourceCode))
  println(s"Res: ${res.state}")
  println(s"Res: ${res.output}")

  def ouputMap(map: Map[Coordinate, Char]): Unit = {

    print("\u001b[2J")

    val keys = map.keySet
    val allX = keys.map(_.x)
    val allY = keys.map(_.y)

    for (y <- allY.min to allY.max) {
      for {x <- allX.min to allX.max} {
        print(map(Coordinate(x, y)))
      }
      println()
    }
  }

  def isIntersection(position: Coordinate, map: Map[Coordinate, Char]): Boolean = {
    val neighbors = List(
      position.copy(x = position.x - 1),
      position.copy(x = position.x + 1),
      position.copy(y = position.y - 1),
      position.copy(y = position.y + 1)
    )
    map(position) == '#' &&
      neighbors.forall(pos => map.get(pos).contains('#'))
  }

  // create map
  val map = res.output.foldLeft((0, 0, Map.empty[Coordinate, Char])){ case ((x, y, map), o) =>
    if (o == 10) (0, y + 1, map) else (x + 1, y, map + (Coordinate(x, y) -> o.toChar))
  }._3

  ouputMap(map)

  val intersections = map.keySet.filter(pos => isIntersection(pos, map))
  val alignmentParamsSum = intersections.map{pos => pos.x * pos.y}.sum
  println(s"Alignment parameters sum: $alignmentParamsSum")

  def computeTurn(position: Coordinate, direction: Direction, map: Map[Coordinate, Char]): Option[Turn] = {
    val (left, right) = direction match {
      case Direction.North => (position.neighbour(Direction.West), position.neighbour(Direction.East))
      case Direction.South => (position.neighbour(Direction.East), position.neighbour(Direction.West))
      case Direction.West => (position.neighbour(Direction.South), position.neighbour(Direction.North))
      case Direction.East => (position.neighbour(Direction.North), position.neighbour(Direction.South))
    }
    println(s"Left: $left - Right: $right")

    if (map.get(left).contains('#')) {
      Some(Turn.Left)
    } else if (map.get(right).contains('#')) {
      Some(Turn.Right)
    } else {
      None
    }
  }

  def turnToChar(turn: Turn): Char = turn match {
    case Turn.Left => 'L'
    case Turn.Right => 'R'
  }

  @tailrec def findNextPosition(position: Coordinate, direction: Direction, map: Map[Coordinate, Char]): Coordinate = {
    val neighbor = position.neighbour(direction)
    if (map.get(neighbor).contains('#')) findNextPosition(neighbor, direction, map) else position
  }

  @tailrec def walkPath(position: Coordinate, direction: Direction, map: Map[Coordinate, Char], path: String): String = {
    println(s"Walking from $position - direction $direction")
    // find turn and length to walk
    computeTurn(position, direction, map) match {
      case None => path
      case Some(turn) =>
        val nextDirection = direction.adjustDirection(turn)
        val nextPosition = findNextPosition(position, nextDirection, map)
        println(s"Next position: $nextPosition - direction: $nextDirection")
        val distance = position.distanceTo(nextPosition)
        val updatedPath = path + turnToChar(turn) + distance
        println(s"Path: $updatedPath")
        walkPath(nextPosition, nextDirection, map, updatedPath)
    }
  }

  val startPosition = map.find(_._2.toChar == '^').map(_._1).getOrElse(sys.error("Start position not found"))
  val singlePath = walkPath(startPosition, Direction.North, map, "")

  println(s"Final Path: $singlePath")
}
