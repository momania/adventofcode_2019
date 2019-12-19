package aoc

import aoc.int.{IntComputer, IntComputerProgress}

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Random

object Day15 extends App {

  val source = Source.fromFile("input/day15.input")
  val sourceCode = source.getLines().mkString.split(',').map(_.toLong).toList

  sealed trait Direction {
    def code: Int
  }
  object Direction {
    case object North extends Direction { lazy val code = 1 }
    case object South extends Direction { lazy val code = 2 }
    case object West extends Direction { lazy val code = 3 }
    case object East extends Direction { lazy val code = 4 }

    val all: List[Direction] = List(North, South, East, West)
    private val allMap = all.map{ d => d.code -> d}.toMap

    def apply(direction: Int): Direction = allMap(direction)
  }

  case class Coordinate(x: Int, y: Int)
  case class FillProgress(progress: IntComputerProgress, position: Coordinate, hitDeadEnd: Boolean = false)

  def ouputGameStatus(position: Coordinate, map: Map[Coordinate, Int]): Unit = {

    print("\u001b[2J")
    val gameGrid = map + (position -> 3)

    val keys = gameGrid.keySet
    val allX = keys.map(_.x)
    val allY = keys.map(_.y)

    for (y <- allY.min to allY.max) {
      for {x <- allX.min to allX.max} {
        val c = gameGrid.getOrElse(Coordinate(x, y), -1) match {
          case -1 => ' '
          case 0 => '='
          case 1 => '·'
          case 2 => '#'
          case 3 => 'o'
        }
        print(c)
      }
      println()
    }
  }

  def computeNextCoordinate(current: Coordinate, direction: Direction): Coordinate =  direction match {
    case Direction.North => current.copy(y = current.y - 1)
    case Direction.South => current.copy(y = current.y + 1)
    case Direction.West => current.copy(x = current.x - 1)
    case Direction.East => current.copy(x = current.x + 1)
  }

  def computeNextMove(lastDirection: Direction, position: Coordinate, map: Map[Coordinate, Int]): Direction = {
    val allPossibleMoves = Direction.all.map{ d => d -> computeNextCoordinate(position, d)}.toMap
    val forwardMove = allPossibleMoves(lastDirection)
    if (map.contains(forwardMove)) {
      val allButWalls = allPossibleMoves.filterNot(m => map.get(m._2).contains(0))
      val (allBack, allFree) = allButWalls.partition(c => map.contains(c._2))
      if (allFree.nonEmpty) {
        allFree.head._1 // move to free
      } else {
        Random.shuffle(allBack).head._1 // move known
      }
    } else {
      lastDirection // just move on
    }
  }

  def nextFreeMoves(position: Coordinate, map: Map[Coordinate, Int]): List[Direction] = {
    val allPossibleMoves = Direction.all.map{ d => d -> computeNextCoordinate(position, d)}.toMap
    val allButWalls = allPossibleMoves.filterNot(m => map.get(m._2).contains(0))
    val (_, allFree) = allButWalls.partition(c => map.contains(c._2))
    allFree.keySet.toList
  }

  @tailrec def findAir(progress: IntComputerProgress, direction: Direction, position: Coordinate, map: Map[Coordinate, Int], path: List[Coordinate]): (IntComputerProgress, Coordinate) = {
    val nextPosition = computeNextCoordinate(position, direction)
    val updatedProgress = IntComputer.runComputer(progress.copy(input = List(direction.code)))
    val moveResult = updatedProgress.output.head.toInt
    val updatedMap = map + (nextPosition -> moveResult)
    moveResult match {
      case 0 => // hit wall, find next move without updating path
        println(s"Stil at $position - path: ${path.length}")
        val nextMove = computeNextMove(direction, position, updatedMap)
        findAir(updatedProgress, nextMove, position, updatedMap, path)
      case 1 => // moved, find next move
        println(s"Moved to $nextPosition - path: ${path.length}")
        val updatedPath = if (path.contains(nextPosition)) path.dropWhile(_ != nextPosition) else nextPosition :: path
        val nextMove = computeNextMove(direction, nextPosition, updatedMap)
        findAir(updatedProgress, nextMove, nextPosition, updatedMap, updatedPath)
      case 2 => // found the oxygen
        ouputGameStatus(Coordinate(0,0), updatedMap)
        println(s"Path length: ${(nextPosition :: path).length}")

        (progress, nextPosition)
    }
  }



  @tailrec def fillMaze(fillProgresses: List[FillProgress], map: Map[Coordinate, Int], minutes: Int): Int = {
    if (fillProgresses.forall(_.hitDeadEnd)) {
      minutes
    } else {
      val updatedFillProgresses = fillProgresses.flatMap { fillProgress =>
        val moves = nextFreeMoves(fillProgress.position, map)
        if (moves.isEmpty) {
          List((fillProgress.copy(hitDeadEnd = true), map))
        } else {
          moves.map { move =>
            val nextPosition = computeNextCoordinate(fillProgress.position, move)
            val updatedProgress = IntComputer.runComputer(fillProgress.progress.copy(input = List(move.code)))
            val moveResult = updatedProgress.output.head.toInt
            val updatedMap = map + (nextPosition -> moveResult)
            moveResult match {
              case 0 => // wall
                (fillProgress.copy(updatedProgress), updatedMap)
              case 1 | 2 => // moved
                (fillProgress.copy(updatedProgress, nextPosition), updatedMap)
            }
          }
        }
      }
      val mergedMaps = updatedFillProgresses.map(_._2).reduce(_ ++ _)
      fillMaze(updatedFillProgresses.map(_._1), mergedMaps, minutes + 1)
    }

  }

  val (progress, position) = findAir(IntComputerProgress(sourceCode), Direction.South, Coordinate(0,0), Map.empty, List.empty)
  println(s"Found the oxygen on $position")

  val minutes = fillMaze(List(FillProgress(progress, position)), Map(position -> 2), 0)
  println(s"Air filled in ${minutes} minutes")
}
