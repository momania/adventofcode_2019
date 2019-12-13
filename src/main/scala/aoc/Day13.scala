package aoc

import aoc.int.{IntComputer, IntComputerProgress}

import scala.annotation.tailrec
import scala.io.Source

object Day13 extends App {

  val source = Source.fromFile("input/day13.input")
  val sourceCode = source.getLines().mkString.split(',').map(_.toLong).toList

  case class Tile(x: Int, y: Int, content: Int)
  case class Cooridinate(x: Int, y: Int)
  case class GameStatus(score: Int, ball: Cooridinate, paddle: Cooridinate, numberOfBlocks: Int)

  def outputToGameStatus(out: List[Long]): GameStatus = {
    val tiles = out.sliding(3, 3).toList.map { case x :: y :: content :: Nil => Tile(x.toInt, y.toInt, content.toInt)}
    val score = tiles.find(t => t.x == -1 && t.y  == 0).map(_.content).getOrElse(0)
    val ball = tiles.find(_.content == 4).map{ t=> Cooridinate(t.x, t.y)}.getOrElse(Cooridinate(0, 0))
    val paddle = tiles.find(_.content == 3).map{ t=> Cooridinate(t.x, t.y)}.getOrElse(Cooridinate(0, 0))
    val numberOfBlocks = tiles.count(_.content == 2)
    GameStatus(score, ball, paddle, numberOfBlocks)
  }

  @tailrec def playGame(progress: IntComputerProgress): GameStatus = {
    val updatedProgress = IntComputer.runComputer(progress)
    val gameStatus = outputToGameStatus(updatedProgress.output)
    println(s"Intermediate game status: $gameStatus - computer state: ${updatedProgress.state}")
    if (gameStatus.numberOfBlocks == 0) {
      gameStatus
    } else {
      val joystickMovement = -1L
      playGame(updatedProgress.copy(input = List(joystickMovement)))
    }
  }

  val singleRun = IntComputer.runComputer(IntComputerProgress(sourceCode))
  val gameStatus = outputToGameStatus(singleRun.output)
  println(s"Number of block tiles: ${gameStatus.numberOfBlocks}")

  val gameResult = playGame(IntComputerProgress(sourceCode.updated(0, 2)))
  println(s"Game result: $gameResult")
}
