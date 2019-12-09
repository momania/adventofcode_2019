package aoc

import java.awt.image.BufferedImage
import java.io.File

import javax.imageio.ImageIO

import scala.io.Source

object Day08 extends App {

  val source = Source.fromFile("input/day08.input")
  val sourceCode = source.getLines().mkString.map(_.toString.toInt).toList

  val (imageX, imageY) = (25, 6)

  val layers = sourceCode.grouped(imageX * imageY).toList
  for (layer <- layers) {
    println(s"Layer: $layer")
  }

  val sortedByZeros = layers.sortBy(_.count(_ == 0))
  for (layer <- sortedByZeros) {
    println(s"Sorted: $layer")
  }

  val check = sortedByZeros.head.count(_ == 1) * sortedByZeros.head.count(_ == 2)
  println(s"Check: $check")


  val stackedLayers = List.fill(imageX * imageY)(-1)
  val layeredImage = layers.reverse.foldLeft(stackedLayers){ case(stack, layer) =>
    stack.zip(layer).map { case(s, l) => if (l < 2) l else math.min(s, l)}
  }

  println(s"Layer: $layeredImage")
  println(s"Has -1: ${layeredImage.contains(-1)}")

  val out = new BufferedImage(imageX, imageY, BufferedImage.TYPE_INT_RGB)
  for ((line, y) <- layeredImage.grouped(imageX).zipWithIndex) {
    for ((color, x) <- line.zipWithIndex) {
      val rgb = colorToRgb(color)
      println(s"Writing $x - $y - $rgb")
      out.setRGB(x, y, rgb)
    }
  }

  ImageIO.write(out, "jpg", new File("test.jpg"))

  def colorToRgb(color: Int) = color match {
    case 0 => 0x000000
    case 1 => 0xffffff
  }
}
