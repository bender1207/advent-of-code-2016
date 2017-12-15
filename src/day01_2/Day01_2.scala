package day01_2

import scala.io.Source

object Day01_2
{
  trait Movement
  case class Left(steps: Int) extends Movement
  case class Right(steps: Int) extends Movement

  case class Position(x: Int, y:Int)
  {
    def +(other: Position) : Position = Position(this.x + other.x, this.y + other.y)
    def distance() : Int = math.abs(x) + math.abs(y)
  }

  case class Direction(x: Int, y: Int)
  {
    def positons(steps: Int) : Seq[Position] = (1 to steps).map(s => Position(x, y))
    def turn(movement: Movement) : Direction =
    {
      movement match {
        case Left(s) => Direction(-y, x)
        case Right(s) => Direction(y, -x)
      }
    }
  }

  def computeDistance(line: String) : Int = {
    val movements = line.split(",").map(_.trim).map(s => if (s.head == 'L') Left(s.tail.toInt) else Right(s.tail.toInt)).toList

    val positions = toIncrementalPositions(movements, Direction(0, 1)).toList
    val position = travel(positions.tail, positions.head, Set[Position]())

    position.distance()
  }

  def toIncrementalPositions(movements: List[Movement], d: Direction) : Seq[Position] =
  {
    if (movements == Nil) Seq() else
    {
      val newDirection = d.turn(movements.head)

      movements.head match
      {
        case Left(steps) => newDirection.positons(steps) ++ toIncrementalPositions(movements.tail, newDirection)
        case Right(steps) => newDirection.positons(steps) ++ toIncrementalPositions(movements.tail, newDirection)
      }
    }
  }

  def travel(positions: List[Position], p: Position, visited: Set[Position]) : Position =
  {
    if (visited(p)) p else {
      val visitedUpdated = visited + p
      travel(positions.tail, p + positions.head, visitedUpdated)
    }
  }



  def main(args: Array[String]) =
  {
    //-- Tests -----------------
    {
      assert(computeDistance("R8, R4, R4, R8") == 4)
    }
    //--------------------------

    // Read input
    //
    val line: String = Source.fromResource("day01.txt").getLines.toSeq.head.trim

    println(computeDistance(line))
  }
}
