package aoc.model

sealed trait Direction {
  def adjustDirection(turn: Turn): Direction
}
object Direction {
  case object North extends Direction {
    override def adjustDirection(turn: Turn): Direction = turn match {
      case Turn.Left => West
      case Turn.Right => East
    }
  }
  case object South extends Direction {
    override def adjustDirection(turn: Turn): Direction = turn match {
      case Turn.Left => East
      case Turn.Right => West
    }
  }
  case object West extends Direction {
    override def adjustDirection(turn: Turn): Direction = turn match {
      case Turn.Left => South
      case Turn.Right => North
    }
  }
  case object East extends Direction {
    override def adjustDirection(turn: Turn): Direction = turn match {
      case Turn.Left => North
      case Turn.Right => South
    }
  }
}
