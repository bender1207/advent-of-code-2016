package day02_2

import scala.io.Source

object Day02_2
{
  object Pad {
    val grid : IndexedSeq[Char] = IndexedSeq(
      '.', '.', '1', '.', '.',
      '.', '2', '3', '4', '.',
      '5', '6', '7', '8', '9',
      '.', 'A', 'B', 'C', '.',
      '.', '.', 'D', '.', '.')

    val valid : IndexedSeq[Boolean] = IndexedSeq(
      false, false, true, false, false,
      false, true, true, true, false,
      true, true, true, true, true,
      false, true, true, true, false,
      false, false, true, false, false
    )

    def isValid(p: Position) : Boolean = p.x >= 0 && p.x < 5 && p.y >= 0 && p.y < 5 && valid(p.x + p.y * 5)
    def getChar(p: Position) : Char = grid(p.x + p.y * 5)
  }

  case class Position(x: Int, y: Int) {
    def +(other: Position) : Position = { Position(this.x + other.x, this.y + other.y) }
  }

  def toPosition(c: Char) : Position =
  {
    c match {
      case 'U' => Position(0, -1)
      case 'D' => Position(0, 1)
      case 'L' => Position(-1, 0)
      case 'R' => Position(1, 0)
    }
  }

  def findCode(rows: Seq[String]) : String =
  {
    buildString(rows.toList, Position(0, 2))
  }

  def getCharacter(line: List[Char], p: Position) : (Char, Position) =
  {
    line match {
      case Nil => (Pad.getChar(p), p)
      case c :: cs => {
        val dp = toPosition(c)
        val pNew = if (Pad.isValid(p + dp)) p + dp else p
        getCharacter(cs, pNew)
      }
    }
  }

  def buildString(rows: List[String], startPos: Position) : String = {
    rows match {
      case Nil => ""
      case s :: ss => {
        val (c, p) = getCharacter(s.toList, startPos)
        c.toString + buildString(ss, p)
      }
    }
  }

  def main(args: Array[String]) =
  {
    //-- Tests -----------------
    {
      assert(findCode(Seq("ULL", "RRDDD", "LURDL", "UUUUD")) == "5DB3")
    }
    //--------------------------

    // Read input
    //
    val rows: Seq[String] = Source.fromResource("day02.txt").getLines.toSeq

    println(findCode(rows))
  }

}
