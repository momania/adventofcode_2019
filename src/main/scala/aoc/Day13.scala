package aoc

import aoc.int.{IntComputer, IntComputerProgress, IntComputerState}

import scala.annotation.tailrec
import scala.io.Source

object Day13 extends App {

  val source = Source.fromFile("input/day13.input")
  val sourceCode = source.getLines().mkString.split(',').map(_.toLong).toList

  case class Tile(x: Int, y: Int, content: Int)
  case class Cooridinate(x: Int, y: Int)
  case class GameStatus(score: Int, ball: Cooridinate, paddle: Cooridinate, blocks: Int, gameGrid: Map[Cooridinate, Int])
  object GameStatus {
    def empty: GameStatus = GameStatus(0, Cooridinate(0,0), Cooridinate(0,0), 0, Map.empty)
  }

  def updateGameStatus(gameStatus: GameStatus, out: List[Long]): GameStatus = {
    val tiles = out.sliding(3, 3).toList.map { case x :: y :: content :: Nil => Tile(x.toInt, y.toInt, content.toInt)}
    val updatedGameGrid = tiles.foldLeft(gameStatus.gameGrid){ case (grid, tile) => grid + (Cooridinate(tile.x, tile.y) -> tile.content)}
    val score = updatedGameGrid.getOrElse(Cooridinate(-1, -0), 0)
    val ball = updatedGameGrid.find(_._2 == 4).map(_._1).getOrElse(Cooridinate(0, 0))
    val paddle = updatedGameGrid.find(_._2 == 3).map(_._1).getOrElse(Cooridinate(0, 0))
    val blocks = updatedGameGrid.count(_._2 == 2)
    GameStatus(score, ball, paddle, blocks, updatedGameGrid)
  }

  def ouputGameStatus(gameStatus: GameStatus): Unit = {

    print("\u001b[2J")

    val keys = gameStatus.gameGrid.keySet
    val allX = keys.map(_.x)
    val allY = keys.map(_.y)

    for (y <- 0 to allY.max) {
      for {x <- 0 to allX.max} {
        val c = gameStatus.gameGrid.getOrElse(Cooridinate(x, y), 0) match {
          case 0 => ' '
          case 1 => '#'
          case 2 => '■'
          case 3 => '_'
          case 4 => 'o'
        }
        print(c)
      }
      println()
    }
  }

  @tailrec def playGame(gameStatus: GameStatus, progress: IntComputerProgress): GameStatus = {
    val updatedProgress = IntComputer.runComputer(progress)
    val updatedGameStatus = updateGameStatus(gameStatus, updatedProgress.output)
    ouputGameStatus(updatedGameStatus)
//    println(s"Intermediate game status: $updatedGameStatus - computer state: ${updatedProgress.state} - Out: ${updatedProgress.output}")
    if (updatedGameStatus.blocks == 0 || updatedProgress.state == IntComputerState.Halted) {
      updatedGameStatus
    } else {
      val joystickMove = updatedGameStatus.ball.x.compareTo(updatedGameStatus.paddle.x)
      playGame(updatedGameStatus, updatedProgress.copy(input = List(joystickMove)))
    }
  }

  val singleRun = IntComputer.runComputer(IntComputerProgress(sourceCode))
  val gameStatus = updateGameStatus(GameStatus.empty, singleRun.output)
  println(s"Number of block tiles: ${gameStatus.blocks}")

  val gameResult = playGame(GameStatus.empty, IntComputerProgress(sourceCode.updated(0, 2)))
  println(s"Game result: $gameResult")
  println(s"Game blocks: ${gameResult.blocks}")
  println(s"Game score: ${gameResult.score}")

  ouputGameStatus(gameResult)
}
