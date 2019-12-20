package aoc.model

case class Coordinate(x: Int, y: Int) {
  def up(by: Int) = Coordinate(x + by, y)
  def down(by: Int) = Coordinate(x - by, y)
  def right(by: Int) = Coordinate(x, y + by)
  def left(by: Int) = Coordinate(x, y - by)
  def distanceTo(other: Coordinate) = math.abs(x - other.x) + math.abs(y - other.y)
  def neighbour(direction: Direction) = direction match {
    case Direction.North => copy(y = y - 1)
    case Direction.South => copy(y = y + 1)
    case Direction.West => copy(x = x - 1)
    case Direction.East => copy(x = x + 1)
  }
}
object Coordinate {
  lazy val zero = Coordinate(0, 0)
}
