package aoc

import scala.annotation.tailrec
import scala.io.Source

object Day14 extends App {

  val source = Source.fromFile("input/day14.test2.input")
  val sourceCode = source.getLines().toList


  case class Element(volume: Int, name: String)
  case class Reaction(from: List[Element], to: Element)

  def createElement(input: String): Element = {
    val volume :: name :: Nil = input.trim.split(' ').toList
    Element(volume.toInt, name)
  }

  def createReaction(line: String): Reaction = {
    val fromString :: toString :: Nil = line.split("=>").toList
    val fromElements = fromString.split(',').toList.map(createElement)
    val toElement = createElement(toString)
    Reaction(fromElements, toElement)
  }

  def groupElements(elements: List[Element]): List[Element] = {
    elements.groupBy(_.name).map { case (name, elems) => Element(elems.map(_.volume).sum, name)}.toList
  }

  @tailrec def computeOre(elementsToCreate: List[Element], reactions: List[Reaction], requiredOre: Int): Int = {
    println(s"Creating: $elementsToCreate")
    val requiredElements = elementsToCreate.flatMap{ e =>
      reactions.find(_.to.name == e.name).toList.flatMap { reaction =>
        val scale = math.ceil(e.volume.doubleValue() / reaction.to.volume).toInt
        println(s"S $scale - R: $reaction")
        reaction.from.map(ee => ee.copy(volume = ee.volume * scale))
      }
    }
    val groupedElements = groupElements(requiredElements)
    println(s"Req: ${groupedElements}")
    val (ore, restElements) = groupedElements.partition(_.name == "ORE")
    println(s"O: $ore")
    println(s"R: $restElements")
    val updatedOre = requiredOre + ore.map(_.volume).sum
    if (restElements.isEmpty) {
      updatedOre
    } else {
      computeOre(restElements, reactions, updatedOre)
    }
  }

  val reactions = sourceCode.map(createReaction)
  for (r <- reactions) {
    println(r)
  }
  val requiredOre = computeOre(List(Element(1, "FUEL")), reactions, 0)
  println(s"Required ORE: $requiredOre")
}
