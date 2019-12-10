package aoc

import scala.io.Source

object Day10 extends App {

  val source = Source.fromFile("input/day10.input")
  val sourceCode = source.getLines().toList

  val width = sourceCode.head.length
  val height = sourceCode.length

  println(s"Width: $width")
  println(s"Height: $height")

  case class Location(x: Int, y: Int, hasAstroid: Boolean)

  val locations = sourceCode.zipWithIndex.map { case (line, y) =>
    line.zipWithIndex.map { case (pos, x) => Location(x, y, (pos == '#')) }
  }

  for (l <- locations) {
    println(s"Location: $l")
  }

  // todo find mechanism to 'look around' for each astroid,
  //  by walking edges and compute all coordinates on path
  // then check if path contains astroid
}
