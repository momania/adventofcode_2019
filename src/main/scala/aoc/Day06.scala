package aoc

import scala.io.Source

object Day06 extends App {

  val source = Source.fromFile("input/day06.input")
  val lines = source.getLines().toList

  case class OrbitalObject(id: String, orbitedBy: List[OrbitalObject])

  val orbitalObjects = lines.map { l =>
    val left :: right :: Nil = l.split(')').toList
    OrbitalObject(left, List(OrbitalObject(right, Nil)))
  }

  def buildTree(oo: OrbitalObject, objects: List[OrbitalObject]): OrbitalObject = {
    val (orbitedBy, rest) = objects.partition(_.id == oo.id)
    oo.copy(orbitedBy = orbitedBy.flatMap(_.orbitedBy).map(ob => buildTree(ob, rest)))
  }

  for { root <- orbitalObjects.find(_.id == "COM")} {
    val others = orbitalObjects.filterNot(_.id == "COM")
    val tree = root.copy(orbitedBy = root.orbitedBy.map(ob => {buildTree(ob, others)}))

    def countOrbits(oo: OrbitalObject, dept: Int): Int = {
      val direct = if (dept > 0) 1 else 0
      val indirect = if (dept > 1) (dept - 1) else 0
      val nested = if (oo.orbitedBy.isEmpty) { 0 } else oo.orbitedBy.map(ob => countOrbits(ob, dept + 1)).sum
      indirect + direct + nested
    }

    println(s"Count: ${countOrbits(tree, 0)}")

    def findPath(oo: OrbitalObject, needle: String, collected: List[String]): List[String] = {
      if (oo.id == needle) {
        collected
      } else {
        val updatedCollected = collected :+ oo.id
        oo.orbitedBy.flatMap(ob => findPath(ob, needle, updatedCollected))
      }
    }

    val youPath = findPath(tree, "YOU", Nil)
    val sanPath = findPath(tree, "SAN", Nil)

    val diff = youPath.diff(sanPath) ++ sanPath.diff(youPath)
    println(s"Transfers: ${diff.size}")
  }
}
