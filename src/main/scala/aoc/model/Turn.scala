package aoc.model

sealed trait Turn
object Turn {
  case object Left extends Turn
  case object Right extends Turn
}
