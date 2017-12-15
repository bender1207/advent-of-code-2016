package day01_1

import scala.io.Source

object Day01_1
{
  trait Movement
  case class Left(steps: Int) extends Movement
  case class Right(steps: Int) extends Movement

  case class Position(x: Int, y:Int) { def +(other: Position) : Position = Position(this.x + other.x, this.y + other.y) }
  case class Direction(x: Int, y: Int) { def *(steps: Int) : Position = Position(steps * x, steps * y) }

  def computeDistance(line: String) : Int = {
    val movements = line.split(",").map(_.trim).map(s => if (s.head == 'L') Left(s.tail.toInt) else Right(s.tail.toInt)).toList
    val endPosition = travel(movements, Position(0, 0), Direction(0, 1))
    math.abs(endPosition.x) + math.abs(endPosition.y)
  }

  def travel(movements: List[Movement], p: Position, d: Direction) : Position = {
    movements match {
      case Left(steps) :: tail => travel(tail, p + Direction(-d.y, d.x) * steps, Direction(-d.y, d.x))
      case Right(steps) :: tail => travel(tail, p + Direction(d.y, -d.x) * steps, Direction(d.y, -d.x))
      case Nil => p
    }
  }

  def main(args: Array[String]) =
  {
    //-- Tests -----------------
    {
      assert(computeDistance("R2, L3") == 5)
      assert(computeDistance("R2, R2, R2") == 2)
      assert(computeDistance("R5, L5, R5, R3") == 12)
    }
    //--------------------------

    // Read input
    //
    val line: String = Source.fromResource("day01.txt").getLines.toSeq.head.trim

    println(computeDistance(line))
  }
}
