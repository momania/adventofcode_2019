package aoc

import aoc.int.{IntComputer, IntComputerProgress, IntComputerState}

import scala.annotation.tailrec
import scala.io.Source

object Day11 extends App {

  val source = Source.fromFile("input/day11.input")
  val sourceCode = source.getLines().mkString.split(',').map(_.toLong).toList

  sealed trait Direction {
    def adjustDirection(code: Int): Direction
  }
  object Direction {
    case object Up extends Direction {
      override def adjustDirection(code: Int): Direction = code match {
        case 0 => Left
        case 1 => Right
      }
    }
    case object Down extends Direction {
      override def adjustDirection(code: Int): Direction = code match {
        case 0 => Right
        case 1 => Left
      }
    }
    case object Left extends Direction {
      override def adjustDirection(code: Int): Direction = code match {
        case 0 => Down
        case 1 => Up
      }
    }
    case object Right extends Direction {
      override def adjustDirection(code: Int): Direction = code match {
        case 0 => Up
        case 1 => Down
      }
    }
  }
  sealed trait Color {
    def code: Int
  }
  object Color {
    case object Black extends Color {
      lazy val code = 0
    }
    case object White extends Color {
      lazy val code = 1
    }

    private val all = List(Black, White)
    def apply(code: Int): Color = all.find(_.code == code).getOrElse(sys.error(s"Unknown color code $code"))
  }
  case class Coordinate(x: Int, y: Int)
  case class Ship(coordinate: Coordinate, direction: Direction)
  case class PaintedLocation(coordinate: Coordinate, color: Color)

  @inline def moveToNextCoordinate(coordinate: Coordinate, direction: Direction) = direction match {
    case Direction.Up => coordinate.copy(y = coordinate.y - 1)
    case Direction.Down => coordinate.copy(y = coordinate.y + 1)
    case Direction.Left => coordinate.copy(x = coordinate.x - 1)
    case Direction.Right  => coordinate.copy(x = coordinate.x + 1)
  }

  @tailrec def collectPath(ship: Ship, progress: IntComputerProgress, collected: Map[Coordinate, Color]): Map[Coordinate, Color] = {
    val inputColor = collected.getOrElse(ship.coordinate, Color.Black)
    val updatedProgress = IntComputer.runComputer(progress.copy(input = List(inputColor.code)))
    val colorCode :: directionCode :: Nil = updatedProgress.output
    val updatedCollected = collected + (ship.coordinate -> Color(colorCode.toInt))
    if (updatedProgress.state != IntComputerState.Halted) {
      val updatedDirection = ship.direction.adjustDirection(directionCode.toInt)
      val updatedCoordinate = moveToNextCoordinate(ship.coordinate, updatedDirection)
      val updatedShip = ship.copy(coordinate = updatedCoordinate, direction = updatedDirection)
      collectPath(updatedShip, updatedProgress, updatedCollected)
    } else {
      updatedCollected
    }
  }

  def paintGrid(grid: Map[Coordinate, Color]): Unit = {
    val keys = grid.keySet
    val allX = keys.map(_.x)
    val allY = keys.map(_.y)

    for (y <- allY.min to allY.max) {
      for {x <- allX.min to allX.max} {
        grid.getOrElse(Coordinate(x, y), Color.Black) match {
          case Color.Black => print(' ')
          case Color.White => print('■')
        }
      }
      println()
    }
  }

  val paintedGrid = collectPath(Ship(Coordinate(0, 0), Direction.Up), IntComputerProgress(sourceCode), Map.empty)
  println(s"Painted at least once: ${paintedGrid.size}")
  paintGrid(paintedGrid)

  val paintedGrid2 = collectPath(Ship(Coordinate(0, 0), Direction.Up), IntComputerProgress(sourceCode), Map(Coordinate(0,0) -> Color.White))
  println(s"Painted grid 2: $paintedGrid2")
  paintGrid(paintedGrid2)

}
