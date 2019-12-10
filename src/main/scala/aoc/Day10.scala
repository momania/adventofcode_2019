package aoc

import scala.io.Source

object Day10 extends App {3

  val source = Source.fromFile("input/day10.input")
  val sourceCode = source.getLines().toList

  val width = sourceCode.head.length
  val height = sourceCode.length

  val xRange = 0 until width
  val yRange = 0 until height
  val verticalEdges = xRange.map { x => Coordinate(0, x)} ++ xRange.map { x => Coordinate(height - 1, x)}
  val horizontalEdges = yRange.map { x => Coordinate(x, 0)} ++ yRange.map { x => Coordinate(x, width - 1)}
  println(s"Width: $width")
  println(s"Height: $height")
  println(s"Vertical edges: $verticalEdges")
  println(s"Horizontal edges: $horizontalEdges")
  val allEdges = (verticalEdges ++ horizontalEdges).distinct

  case class Coordinate(x: Int, y: Int)
  case class Location(coordinate: Coordinate, hasAstroid: Boolean)
  case class Path(a: Double, b: Double, direction: Int, path: Seq[Coordinate])

  val grid = sourceCode.zipWithIndex.flatMap { case (line, y) =>
    line.zipWithIndex.map { case (pos, x) => Location(Coordinate(x, y), (pos == '#')) }
  }

  println(s"GRID: $grid")

  val astroidLocations = grid.filter(_.hasAstroid)
  val locationsWithNumbers = astroidLocations.map { location => location -> numberOfAstroidsInView(location.coordinate, astroidLocations)}
  val bestPoint = locationsWithNumbers.maxBy(_._2)

  println(s"Locations with numbers: $locationsWithNumbers")
  println(s"Location with most in sight: ${bestPoint}")

  def numberOfAstroidsInView(coordinate: Coordinate, astroidLocations: List[Location]): Int = {
    val allMinusSelf = astroidLocations.filterNot(_.coordinate == coordinate)

    val allPaths = allMinusSelf.map {l => calculatePath(coordinate, l.coordinate)}
    allPaths.map { p => (p.a, p.b, p.direction) }.distinct.size // just compute distinct directional paths
  }

  def filterEdges(coordinate: Coordinate): Seq[Coordinate] = {
    println(s"Filtering coordinate $coordinate")
    if (allEdges.contains(coordinate)) {
      val filteredSelf = allEdges.filterNot(_ == coordinate)
      val filteredVerticals = filteredSelf.filterNot(edge => edge.x == coordinate.x && edge.y != 0 && edge.y != height -1)
      filteredVerticals.filterNot(edge => edge.y == coordinate.y && edge.x != 0 && edge.x != width -1)
    } else {
      allEdges
    }
  }

  def calculatePath(position: Coordinate, target: Coordinate): Path = {
    val path = if (position.x == target.x) {
      val direction = (target.y - position.y).sign
      Path(position.x, Double.PositiveInfinity, direction, (position.y.to(target.y, (target.y - position.y).sign)).map { y => Coordinate(position.x, y)})
    } else {
      val a = (position.y - target.y).toDouble / (position.x - target.x)
      val b = position.y - (a * position.x)
      val p = position.x.to(target.x, (target.x - position.x).sign).flatMap { x =>
        val y = (a * x) + b
        if (y == math.rint(y)) {
          Some(Coordinate(x, y.toInt))
        }else {
          None
        }
      }
      val direction = (target.x - position.x).sign
      Path(a, b, direction, p)
    }
    path.copy(path = (path.path.filterNot(_ == position) :+ target).distinct)
  }
}
