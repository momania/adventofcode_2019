package aoc

import scala.annotation.tailrec
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
  case class Path(angle: Double, target: Coordinate, distance: Double)

  val grid = sourceCode.zipWithIndex.flatMap { case (line, y) =>
    line.zipWithIndex.map { case (pos, x) => Location(Coordinate(x, y), (pos == '#')) }
  }

  println(s"GRID: $grid")

  val astroidLocations = grid.filter(_.hasAstroid)
  val locationsWithNumbers = astroidLocations.map { location => location -> numberOfAstroidsInView(location.coordinate, astroidLocations)}
  val bestPoint = locationsWithNumbers.maxBy(_._2)

  println(s"Locations with numbers: $locationsWithNumbers")
  println(s"Location with most in sight: ${bestPoint}")


  val pathsForBestPoint = pathsForCoordinate(bestPoint._1.coordinate, astroidLocations)
  println(s"Paths for best: $pathsForBestPoint")
  val grouped = pathsForBestPoint.groupBy(_.angle).toList

  println(s"G: $grouped")
  val sorted = grouped.sortBy(_._1).reverse
  println(s"Sorted: ${sorted}")


  val sortedPaths = sorted.map(_._2.sortBy(_.distance))

  @tailrec def collectPaths(sortedPaths: List[List[Path]], collected: List[Path]): List[Path] = {
    val scraped = sortedPaths.map{_.head}
    val rest = sortedPaths.map(_.tail).filterNot(_.isEmpty)
    if (rest.isEmpty) {
      collected ::: scraped
    } else {
      collectPaths(rest, collected ::: scraped)
    }
  }

  val flattenedPaths = collectPaths(sortedPaths, Nil)
  val indexedPAths: List[(Path, Int)] = flattenedPaths.zipWithIndex

  for (elem <- indexedPAths.find(_._2 == 199).map(_._1)) {
    val bla = (elem.target.x * 100) + elem.target.y
    println(s"Ouput: $bla @ $elem")
  }


  def numberOfAstroidsInView(coordinate: Coordinate, astroidLocations: List[Location]): Int = {
    val allPaths = pathsForCoordinate(coordinate, astroidLocations)
//    println(s"C: $coordinate -  Paths: $allPaths")
    allPaths.map { _.angle }.distinct.size // just compute distinct directional paths
  }

  def pathsForCoordinate(coordinate: Coordinate, astroidLocations: List[Location]): List[Path] = {
    val allMinusSelf = astroidLocations.filterNot(_.coordinate == coordinate)
    allMinusSelf.map {l => calculatePath(coordinate, l.coordinate)}
  }


  def calculatePath(position: Coordinate, target: Coordinate): Path = {
    val distance = math.sqrt(math.pow(position.x - target.x, 2) + math.pow(position.y - target.y, 2))
    val angle = (math.atan2(position.y - target.y, target.x - position.x) * 180 / math.Pi)
    val correctedAngle = if (angle <= 90d) angle + 360 else angle
    Path(correctedAngle, target, distance)
  }
}
