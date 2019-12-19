package aoc

import aoc.int.{IntComputer, IntComputerProgress}

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Random

object Day15 extends App {

  val source = Source.fromFile("input/day15.input")
  val sourceCode = source.getLines().mkString.split(',').map(_.toLong).toList

  case class Coordinate(x: Int, y: Int)

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

  def computeNextCoordinate(current: Coordinate, move: Int): Coordinate =  move match {
    case 1 => current.copy(y = current.y - 1)
    case 2 => current.copy(y = current.y + 1)
    case 3 => current.copy(x = current.x - 1)
    case 4 => current.copy(x = current.x + 1)
  }

  def computeNextMove(lastMove: Int, position: Coordinate, map: Map[Coordinate, Int]): Int = {
//    ouputGameStatus(position, map)
    println(s"Current pos: $position")
    val allPossibleMoves = List(1, 2, 3, 4).map{ m=> m -> computeNextCoordinate(position, m)}.toMap
    val forwardMove = allPossibleMoves(lastMove)
    if (map.contains(forwardMove)) {
      val allButWalls = allPossibleMoves.filterNot(m => map.get(m._2).contains(0))
      val (allBack, allFree) = allButWalls.partition(c => map.contains(c._2))
      if (allFree.nonEmpty) {
        println(s"Trying to move to ${allFree.head._2}")
        allFree.head._1 // move to free
      } else {
        println(s"Moving back to ${allBack.head._2}")
        Random.shuffle(allBack).head._1 // move known
      }
    } else {
      println(s"Moving forward to ${forwardMove}")
      lastMove // just move on
    }
  }

  @tailrec def findAir(progress: IntComputerProgress, move: Int, position: Coordinate, map: Map[Coordinate, Int]): (Map[Coordinate, Int], Coordinate) = {
    val nextPosition = computeNextCoordinate(position, move)
    val updatedProgress = IntComputer.runComputer(progress.copy(input = List(move)))
    val moveResult = updatedProgress.output.head.toInt
    val updatedMap = map + (nextPosition -> moveResult)
    moveResult match {
      case 0 => // hit wall, find next move without updating path
        val nextMove = computeNextMove(move, position, updatedMap)
        findAir(updatedProgress, nextMove, position, updatedMap)
      case 1 => // moved, find next move
        val nextMove = computeNextMove(move, nextPosition, updatedMap)
        findAir(updatedProgress, nextMove, nextPosition, updatedMap)
      case 2 => // found the oxygen
        (updatedMap, nextPosition)

    }
  }

  val (map, position) = findAir(IntComputerProgress(sourceCode), 4, Coordinate(0,0), Map.empty)
  ouputGameStatus(Coordinate(0,0), map)
  println(s"Found the oxygen on $position")
}
