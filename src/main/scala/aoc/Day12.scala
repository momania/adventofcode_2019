package aoc

import scala.annotation.tailrec
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.io.Source

object Day12 extends App {

  val source = Source.fromFile("input/day12.input")
  val sourceCode = source.getLines().toList

  val names = List("Io", "Europa", "Ganymede", "Callisto")

  case class Measure(x: Int, y: Int, z: Int)
  case class Moon(name: String, position: Measure, velocity: Measure)

  def computeGravity(left: Measure, right: Measure): Measure = {
    val x = left.x.compareTo(right.x)
    val y = left.y.compareTo(right.y)
    val z = left.z.compareTo(right.z)
    Measure(x, y, z)
  }

  def inverseMeasure(measure: Measure): Measure = {
    Measure(-measure.x, -measure.y, -measure.z)
  }

  def applyGravity(moon: Moon, gravity: Measure): Moon = {
    val v = moon.velocity
    val updatedVelocity = moon.velocity.copy(x = v.x + gravity.x, y = v.y + gravity.y, z = v.z + gravity.z)
    moon.copy(velocity = updatedVelocity)
  }

  def applyVelocity(moon: Moon): Moon = {
    val p = moon.position
    val v = moon.velocity
    val updatedPosition = moon.position.copy(x = p.x + v.x, y = p.y + v.y, z = p.z + v.z)
    moon.copy(position = updatedPosition)
  }

  def runSimulation(moons: List[Moon]): List[Moon] = {
    val combinations = moons.combinations(2).map{ case left :: right :: Nil => left -> right}
    val gravityUpdatedMoons = combinations.foldLeft(moons) { case (moons, (left, right)) =>
      val gravityDiff = computeGravity(left.position, right.position)
      val (current, rest) =  moons.partition(m => m.name == left.name || m.name == right.name)
      val updatedLeft = current.find(_.name == left.name).map(m => applyGravity(m, inverseMeasure(gravityDiff))).toList
      val updatedRight = current.find(_.name == right.name).map(m => applyGravity(m, gravityDiff)).toList
      updatedLeft ::: updatedRight ::: rest
    }
    gravityUpdatedMoons.map(applyVelocity)
  }

  @tailrec def stepSimulation(steps: Int, moons: List[Moon]): List[Moon] = {
    if (steps > 0) {
      stepSimulation(steps - 1, runSimulation(moons))
    } else {
      moons
    }
  }

  def computeTotalEnergy(moons: List[Moon]): Long = {
    moons.map { moon =>
      computeMeasureEnergy(moon.position) * computeMeasureEnergy(moon.velocity)
    }.sum
  }

  def computeMeasureEnergy(measure: Measure): Long = {
    math.abs(measure.x) + math.abs(measure.y) + math.abs(measure.z)
  }

  @tailrec def stepsUntilSecondObservation(moons: List[Moon], observed: List[List[Int]], extractPosition: Moon => Int, extracVelocity: Moon => Int): Long = {
    val updatedMoons = runSimulation(moons)
    val addingObserved = updatedMoons.map(extractPosition) ::: updatedMoons.map(extracVelocity)
//    val velocity = updatedMoons.map(extracVelocity)
    if (observed.contains(addingObserved)) {
      println("Found second observation:")
      for (m <- updatedMoons) {
        println(m)
      }
      observed.size
    } else {
      stepsUntilSecondObservation(updatedMoons, observed :+ addingObserved, extractPosition, extracVelocity)
    }
  }


  val moons = sourceCode.zipWithIndex.map{ case (line, index) =>
    val elems = line.tail.dropRight(1)
    val x :: y :: z :: Nil = elems.split(',').map { e => e.split('=').last.toInt}.toList
    Moon(names(index), Measure(x, y, z), Measure(0, 0, 0))
  }

  println(s"Initial Moons:")
  for (m <- moons.sortBy(_.name)) {
    println(m)
  }

  val steps = 1000
  val simulatedMoons = stepSimulation(steps, moons)
  val totalEnergy = computeTotalEnergy(simulatedMoons)
  println(s"Moons after $steps steps:")
  for (m <- simulatedMoons.sortBy(_.name)) {
    println(m)
  }
  println(s"Total energy: $totalEnergy")

  import scala.concurrent.ExecutionContext.Implicits._

  val stepsForX = Future {
    println("Searching for X")
    stepsUntilSecondObservation(moons, List.empty, _.position.x, _.velocity.x)
  }

  val stepsForY = Future {
    println("Searching for Y")
    stepsUntilSecondObservation(moons, List.empty, _.position.y, _.velocity.y)
  }

  val stepsForZ = Future {
    println("Searching for Z")
    stepsUntilSecondObservation(moons, List.empty, _.position.z, _.velocity.z)
  }

  @tailrec def gcd(a: Long,b: Long): Long = {
    if(b == 0L) a else gcd(b, a%b)
  }

  val totals = Await.result(Future.sequence(List(stepsForX, stepsForY, stepsForZ)), Duration.Inf)
  println(s"Steps taken until second observation $totals")
  val total = totals.reduce { (a, b) => (math.abs(a * b) / gcd(a, b)) }
  println(s"""Total: $total""")
}
