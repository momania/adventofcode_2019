package aoc

object Day04 extends App {

  val input = (264793 to 803935).map(_.toString)

  val validPasswords = input.filter(isValidPassword)
  println(s"Total valid passwords: ${validPasswords.size}")

  val validPasswordsTwo = input.filter(isValidPasswordPartTwo)
  println(s"Total valid passwords (part 2): ${validPasswordsTwo.size}")

  def isValidPassword(in: String) = {
    val sliding = in.toList.sliding(2, 1)
    val hasPair = sliding.exists {
      _.distinct.size == 1
    }
    isIncreasing(in) && hasPair
  }

  def isValidPasswordPartTwo(in: String) = {
    val sliding = in.toList.sliding(2, 1).toList
    val grouped = sliding.groupBy(identity)
    val hasUniqueGroup = grouped.exists { case (key, value) =>
      key.distinct.size == 1 && value.size == 1
    }
    isIncreasing(in) && hasUniqueGroup
  }

  def isIncreasing(in: String): Boolean = {
    in.toList.sliding(2, 1).forall {
      case x :: y :: Nil => x <= y
    }
  }
}
